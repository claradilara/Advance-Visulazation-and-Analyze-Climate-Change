# ---- 0) load dataset ----
# Parse dates and coerce numerics (disasters)
library(tidyverse)
library(sf)
library(rnaturalearth); library(rnaturalearthdata)
library(lubridate)
library(scales)
library(forcats)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(wesanderson)
library(ggiraph)
library(janitor)


# ---- 0) Load & prepare data -------------------------------------------------

tweets_raw    <- readr::read_csv("C:/Users/ozdil/OneDrive/Desktop/Third Semester/Advanced Visualisation in R/The Climate Change Twitter Dataset.csv",  show_col_types = FALSE )%>% clean_names()


disasters_raw <- readr::read_csv("C:/Users/ozdil/OneDrive/Desktop/Third Semester/Advanced Visualisation in R/disasters.csv",show_col_types = FALSE
) %>% clean_names()



disasters <- disasters_raw %>%
  mutate(
    start_date = suppressWarnings(lubridate::ymd(start_date)),
    end_date   = suppressWarnings(lubridate::ymd(end_date)),
    end_date   = if_else(is.na(end_date), start_date, end_date),
    event_mid_date = start_date + floor((end_date - start_date)/2),
    event_duration_days = as.integer(end_date - start_date) + 1L,
    latitude   = as.numeric(latitude),  longitude = as.numeric(longitude),
    total_deaths = as.numeric(total_deaths),
    no_affected  = as.numeric(no_affected),
    reconstruction_costs_000_us = as.numeric(`reconstruction_costs_000_us`),
    total_damages_000_us        = as.numeric(`total_damages_000_us`),
    cpi = as.numeric(cpi)
  ) %>%
  # Convert '000 US$ to US$ and CPI-deflate to base=100
  mutate(
    recon_usd_nominal  = reconstruction_costs_000_us * 1000,
    damages_usd_nominal= total_damages_000_us * 1000,
    recon_usd_real     = if_else(!is.na(cpi) & cpi > 0, recon_usd_nominal  * (100 / cpi), recon_usd_nominal),
    damages_usd_real   = if_else(!is.na(cpi) & cpi > 0, damages_usd_nominal* (100 / cpi), damages_usd_nominal),
    fatality_rate      = if_else(!is.na(no_affected) & no_affected > 0, total_deaths / no_affected, NA_real_),
    # Composite severity (z-scored, simple sum)
    severity_index     = scale(damages_usd_real) + scale(total_deaths)
  )


glimpse(disasters)
# Parse tweets
tweets <- tweets_raw %>%
  mutate(
    created_at = suppressWarnings(lubridate::ymd_hms(created_at, quiet = TRUE)),
    tweet_date = as.Date(created_at),
    lat  = as.numeric(lat),  lng = as.numeric(lng),
    temperature_avg = as.numeric(temperature_avg),
    aggressiveness  = as.numeric(aggressiveness),
    sentiment = forcats::fct_relevel(factor(sentiment), c("negative","neutral","positive")),
    stance    = factor(stance),
    topic     = factor(topic),
    gender    = factor(gender),
    sentiment_num = case_when(
      sentiment == "negative" ~ -1,
      sentiment == "neutral"  ~  0,
      sentiment == "positive" ~  1,
      TRUE ~ NA_real_
    )
  )

glimpse(tweets)
# ---- 0) Packages ----
pkgs <- c(
  "tidyverse","lubridate","scales","ggtext","ggridges","treemapify",
  "rnaturalearth","rnaturalearthdata","sf","hexbin","patchwork",
  "countrycode","purrr","tidyr","forcats"
)
to_install <- pkgs[!pkgs %in% installed.packages()[,1]]
if(length(to_install)) install.packages(to_install, Ncpus = max(1, parallel::detectCores()-1))
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- 1) Grand Budapest Hotel palette + theme ----
# Discrete palette inspired by 'wesanderson' GrandBudapest1/2
gp_cols <- c(
  "pink"     = "#E6A0C4",
  "coral"    = "#FD6467",
  "gold"     = "#F1BB7B",
  "mustard"  = "#F2AD00",
  "burgundy" = "#5B1A18",
  "lavender" = "#C6CDF7",
  "blue"     = "#7294D4",
  "sand"     = "#D8A499"
)
gp_bg   <- "#FFF1F5"  # pastel hotel-pink background
gp_grid <- "#EBDDE2"  # delicate gridlines

scale_gp_d_fill  <- function(...) scale_fill_manual(values = unname(gp_cols), ...)
scale_gp_d_color <- function(...) scale_color_manual(values = unname(gp_cols), ...)
scale_gp_c_fill  <- function(...) scale_fill_gradientn(colours = c(gp_cols["lavender"], gp_cols["pink"], gp_cols["coral"], gp_cols["burgundy"]), ...)
scale_gp_c_color <- function(...) scale_color_gradientn(colours = c(gp_cols["lavender"], gp_cols["pink"], gp_cols["coral"], gp_cols["burgundy"]), ...)

theme_grand_budapest <- function(base_size = 12, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background   = element_rect(fill = gp_bg, color = NA),
      panel.background  = element_rect(fill = gp_bg, color = NA),
      panel.grid.major  = element_line(color = gp_grid, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(color = gp_cols["burgundy"], face = "bold"),
      axis.text         = element_text(color = "#6B5E62"),
      plot.title        = element_text(color = gp_cols["burgundy"], face = "bold", size = base_size * 1.35, lineheight = 1.1),
      plot.subtitle     = element_text(color = "#6B5E62"),
      plot.caption      = element_text(color = "#8A7E81"),
      legend.position   = "top",
      legend.title      = element_text(color = gp_cols["burgundy"]),
      legend.text       = element_text(color = "#6B5E62"),
      strip.text        = element_text(face = "bold", color = gp_cols["burgundy"], margin = margin(3,3,3,3)),
      strip.background  = element_rect(fill = alpha(gp_cols["lavender"], .35), color = NA)
    )
}
theme_set(theme_grand_budapest())

gp_title <- function(txt) glue::glue("<span style='color:{gp_cols['burgundy']}'><b>{txt}</b></span>")

# Utilities
not_na_prop <- function(x) mean(!is.na(x))
badge_missing <- function(x) {
  pct <- scales::percent(mean(is.na(x)))
  glue::glue("Missing: {pct}")
}

# ---- 2) DISASTERS: one specialized plot per column ----

# 2.1 categorical helpers
plot_disaster_type <- function(df) {
  df |>
    count(disaster_type, sort = TRUE) |>
    mutate(disaster_type = fct_reorder(disaster_type, n)) |>
    ggplot(aes(n, disaster_type)) +
    geom_col(fill = gp_cols["coral"], width = .8) +
    geom_text(aes(label = scales::comma(n)), hjust = -0.15, size = 3.2, color = gp_cols["burgundy"]) +
    scale_x_continuous(expand = expansion(mult = c(.02, .15))) +
    labs(title = "Disaster types", x = "Events", y = NULL, caption = "Ordered by count") +
    theme_grand_budapest()
}

plot_disaster_type(disasters)

plot_disaster_subtype <- function(df) {
  df |>
    count(disaster_subtype, sort = TRUE) |>
    slice_head(n = 15) |>
    mutate(disaster_subtype = fct_reorder(disaster_subtype, n)) |>
    ggplot(aes(n, disaster_subtype)) +
    geom_segment(aes(x = 0, xend = n, y = disaster_subtype, yend = disaster_subtype), color = gp_cols["lavender"], linewidth = 2) +
    geom_point(color = gp_cols["blue"], size = 4) +
    labs(title = "Top 15 subtypes (lollipop)", x = "Events", y = NULL) +
    theme_grand_budapest()
}

plot_disaster_subtype(disasters)

plot_disaster_group <- function(df) {
  d <- df |> count(disaster_group) |>
    mutate(p = n/sum(n), lab = paste0(disaster_group, " ", scales::percent(p)))
  ggplot(d, aes(x = 2, y = p, fill = disaster_group)) +
    geom_col(width = 1, color = gp_bg, linewidth = .5) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_gp_d_fill(name = NULL) +
    labs(title = "Disaster group share (donut)", subtitle = "Proportion of events by group", x = NULL, y = NULL) +
    theme_grand_budapest() +
    theme(axis.text = element_blank(), axis.ticks = element_blank()) +
    geom_text(aes(label = lab, y = cumsum(p) - p/2), x = 2, color = gp_cols["burgundy"], size = 3.5)
}

plot_disaster_group(disasters)


install.packages('treemapify')

library(treemapify)
plot_disaster_subgroup <- function(df) {
  df |> count(disaster_subgroup, sort = TRUE) |>
    ggplot(aes(area = n, fill = disaster_subgroup, label = paste0(disaster_subgroup,"\n", scales::comma(n)))) +
    treemapify::geom_treemap(color = gp_bg, linewidth = .6) +
    treemapify::geom_treemap_text(family = "serif", color = gp_cols["burgundy"], place = "centre", grow = TRUE, reflow = TRUE) +
    scale_gp_d_fill(name = NULL) +
    labs(title = "Subgroup treemap", subtitle = "Area ∝ number of events") +
    theme_grand_budapest() + theme(legend.position = "none")
}
plot_disaster_subgroup(disasters)

plot_event_name <- function(df) {
  df |>
    mutate(event_name = fct_lump(fct_explicit_na(event_name, "Unknown"), 20)) |>
    count(event_name, sort = TRUE) |>
    mutate(event_name = fct_reorder(event_name, n)) |>
    ggplot(aes(n, event_name)) +
    geom_col(fill = gp_cols["gold"]) +
    labs(title = "Top event names", x = "Events", y = NULL, subtitle = "Unknowns grouped explicitly") +
    theme_grand_budapest()
}

plot_event_name(disasters)

gp_cols <- c(
  "pink"     = "#E6A0C4",
  "coral"    = "#FD6467",
  "gold"     = "#F1BB7B",
  "mustard"  = "#F2AD00",
  "burgundy" = "#5B1A18",
  "lavender" = "#C6CDF7",
  "blue"     = "#7294D4",
  "sand"     = "#D8A499",
  "teal"     = "#1F7A8C"   
)

grDevices::col2rgb(gp_cols[["teal"]])

library(dplyr)


plot_origin <- function(df) {
  df |>
    dplyr::mutate(origin = forcats::fct_lump(forcats::fct_explicit_na(origin, "Unknown"), 4)) |>
    dplyr::count(origin, sort = TRUE) |>
    ggplot2::ggplot(ggplot2::aes(
      area  = n,
      fill  = origin,
      label = paste0(origin, "\n", scales::comma(n))
    )) +
    treemapify::geom_treemap(colour = gp_bg) +
    treemapify::geom_treemap_text(
      colour = gp_cols[["teal"]], family = "serif",
      reflow = TRUE, grow = TRUE
    ) +
    scale_gp_d_fill(name = NULL) +
    ggplot2::labs(title = "Origins (top 5)", subtitle = "Treemap of cause phrases") +
    theme_grand_budapest() +
    ggplot2::theme(legend.position = "none")
}


plot_origin(disasters)



# 2.2 geo
install.packages('countrycode')
library(countrycode)

plot_country_map <- function(df) {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_transform(4326) |>
    dplyr::mutate(
      iso3c = countrycode::countrycode(name_long, "country.name", "iso3c", warn = FALSE)
    )
  
  counts <- df |>
    dplyr::mutate(
      iso3c = countrycode::countrycode(country, "country.name", "iso3c", warn = FALSE)
    ) |>
    dplyr::count(iso3c, name = "events")
  
  world |>
    dplyr::left_join(counts, by = "iso3c") |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = events)) +
    scale_gp_c_fill(
      na.value = scales::alpha(gp_cols[["lavender"]], 0.25),
      labels   = scales::comma
    ) +
    ggplot2::labs(
      title = "Disaster events by country (choropleth)",
      fill  = "Events"
    ) +
    theme_grand_budapest() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank())
}


plot_country_map(disasters)

# plot_location_top <- function(df) {
#   df |>
#     dplyr::mutate(location = forcats::fct_lump(forcats::fct_explicit_na(location, "Unknown"), 20)) |>
#     dplyr::count(location, sort = TRUE) |>
#     dplyr::mutate(location = forcats::fct_reorder(location, n)) |>
#     ggplot2::ggplot(aes(n, location)) +
#     ggplot2::geom_col(fill = gp_cols["sand"]) +
#     ggplot2::labs(title = "Top locations (string field)", x = "Events", y = NULL) +
#     theme_grand_budapest()
#   
# }

# plot_location_top(disasters)

# 2.3 numeric/time helpers
plot_lat <- function(df) {
  df |>
    filter(between(latitude, -90, 90)) |>
    ggplot(aes(x = latitude)) +
    geom_density(fill = gp_cols["lavender"], color = gp_cols["blue"], linewidth = .7, alpha = .8) +
    labs(title = "Latitude distribution", x = "Latitude") +
    theme_grand_budapest()
}
plot_lat(disasters)

plot_lng <- function(df) {
  df |>
    filter(between(longitude, -180, 180)) |>
    ggplot(aes(x = longitude)) +
    geom_density(fill = gp_cols["lavender"], color = gp_cols["blue"], linewidth = .7, alpha = .8) +
    labs(title = "Longitude distribution", x = "Longitude") +
    theme_grand_budapest()
}
plot_lng(disasters)

plot_start_month <- function(df) {
  df |>
    dplyr::mutate(month = lubridate::floor_date(start_date, "month")) |>
    dplyr::count(month, name = "events") |>
    ggplot2::ggplot(ggplot2::aes(x = month, y = events)) +
    ggplot2::geom_col(fill = gp_cols[["pink"]]) +
    ggplot2::labs(title = "Monthly event starts", x = NULL, y = "Events") +
    theme_grand_budapest()
}


plot_start_month(disasters)

plot_end_month <- function(df) {
  df |>
    dplyr::mutate(month = lubridate::floor_date(end_date, "month")) |>
    dplyr::count(month, name = "events") |>
    ggplot2::ggplot(ggplot2::aes(x = month, y = events)) +
    ggplot2::geom_col(fill = gp_cols[["pink"]]) +
    ggplot2::labs(title = "Monthly event ends", x = NULL, y = "Events") +
    theme_grand_budapest()
}


plot_end_month(disasters)


plot_deaths <- function(df) {
  # 本地缺失徽标
  badge_missing_local <- function(x) {
    n <- sum(is.na(x))
    N <- length(x)
    if (N == 0) return("No data")
    if (n == 0) return("No missing")
    sprintf("Missing: %s/%s (%.1f%%)",
            scales::comma(n), scales::comma(N), 100 * n / N)
  }
  
  subtitle_txt <- badge_missing_local(df$total_deaths)
  
  # 线性轴：允许 0，过滤掉非有限值与负数（负死亡不合理）
  df_plot <- df |>
    dplyr::filter(is.finite(total_deaths), total_deaths >= 0)
  
  # SI 格式化（兼容 scales >= 1.2）
  si_labeller <- tryCatch(
    scales::label_number(scale_cut = scales::cut_si("")),
    error = function(e) scales::label_number()
  )
  
  if (nrow(df_plot) == 0) {
    return(
      ggplot() +
        labs(
          title = "Total deaths",
          subtitle = paste(subtitle_txt, "| no non-negative finite values"),
          x = "Deaths"
        ) +
        theme_grand_budapest()
    )
  }
  
  ggplot(df_plot, aes(x = total_deaths)) +
    geom_histogram(fill = gp_cols["lavender"], bins = 30, na.rm = TRUE) +
    geom_rug(aes(x = total_deaths), sides = "b", alpha = .2,
             color = gp_cols["burgundy"], na.rm = TRUE) +
    scale_x_continuous(
      labels = si_labeller,
      breaks = scales::breaks_extended(n = 6)  # 让刻度更匀称
    ) +
    labs(
      title = "Total deaths",
      subtitle = subtitle_txt,
      x = "Deaths"
    ) +
    theme_grand_budapest()
}



plot_deaths(disasters)



install.packages('ggridges')
library(ggridges)

plot_affected_ridge <- function(df) {
  df |>
    filter(!is.na(no_affected), !is.na(disaster_type), no_affected > 0) |>
    mutate(no_affected_log10 = log10(no_affected)) |>
    ggplot(aes(no_affected_log10, y = fct_lump(disaster_type, 8), fill = fct_lump(disaster_type, 8))) +
    ggridges::geom_density_ridges(alpha = .85, color = gp_bg, scale = 1.2) +
    scale_gp_d_fill(name = "Disaster type") +
    labs(title = "Affected (log10) by disaster type", x = "log10(affected)", y = NULL, subtitle = badge_missing(df$no_affected)) +
    theme_grand_budapest() + theme(legend.position = "none")
}
plot_affected_ridge(disasters)


plot_cpi_time <- function(df) {
  df |>
    filter(!is.na(event_mid_date), !is.na(cpi)) |>
    ggplot(aes(event_mid_date, cpi)) +
    geom_line(color = gp_cols["blue"], linewidth = 1) +
    labs(title = "CPI over event-mid dates", x = NULL, y = "CPI") +
    theme_grand_budapest()
}

plot_cpi_time(disasters)

plot_middate_rolling <- function(df) {
  d <- df |>
    mutate(month = floor_date(event_mid_date, "month")) |>
    count(month) |>
    arrange(month) |>
    mutate(n_roll3 = zoo::rollmean(n, k = 3, fill = NA, align = "right"))
  ggplot(d, aes(month, n_roll3)) +
    geom_line(color = gp_cols["coral"], linewidth = 1) +
    labs(title = "3-month rolling events (mid date)", x = NULL, y = "Events (roll-3)") +
    theme_grand_budapest()
}

plot_middate_rolling(disasters)


plot_duration <- function(df) {
  med <- median(df$event_duration_days, na.rm = TRUE)
  ggplot(df, aes(event_duration_days)) +
    geom_histogram(binwidth = 1, boundary = 0, fill = gp_cols["gold"], color = gp_bg) +
    geom_vline(xintercept = med, color = gp_cols["burgundy"], linetype = "dashed") +
    labs(title = "Event duration (days)", subtitle = paste("Median:", med), x = "Days") +
    theme_grand_budapest()
}

plot_duration(disasters)

plot_rate <- function(df) {
  ggplot(df, aes(fatality_rate)) +
    geom_density(fill = gp_cols["pink"], alpha = .9) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(title = "Fatality rate distribution", x = "Rate (0–1)", subtitle = badge_missing(df$fatality_rate)) +
    theme_grand_budapest()
}

plot_rate(disasters)

plot_severity_heat <- function(df) {
  # Expect each row to hold a 32×1 matrix; compute mean by component per disaster_type
  si_long <- df |>
    mutate(row = row_number()) |>
    filter(!map_lgl(severity_index, is.null)) |>
    transmute(disaster_type,
              comp = map(severity_index, ~as.numeric(.x) |> setNames(seq_along(.x)))) |>
    unnest(comp) |>
    mutate(component = as.integer(names(comp)), value = as.numeric(comp)) |>
    select(-comp)
  
  agg <- si_long |>
    group_by(disaster_type, component) |>
    summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")
  
  ggplot(agg, aes(x = factor(component), y = fct_lump(disaster_type, 8), fill = mean_value)) +
    geom_tile(color = gp_bg) +
    scale_gp_c_fill(name = "Mean") +
    labs(title = "Severity index heatmap", x = "Component (1..32)", y = "Disaster type") +
    theme_grand_budapest()
}

plot_severity_heat(disasters)

# Dispatcher for disasters
plots_disasters <- list(
  disaster_type               = plot_disaster_type(disasters),
  disaster_subtype            = plot_disaster_subtype(disasters),
  disaster_group              = plot_disaster_group(disasters),
  disaster_subgroup           = plot_disaster_subgroup(disasters),
  event_name                  = plot_event_name(disasters),
  origin                      = plot_origin(disasters),
  country                     = plot_country_map(disasters),
  # location                    = plot_location_top(disasters),
  latitude                    = plot_lat(disasters),
  longitude                   = plot_lng(disasters),
  start_date                  = plot_start_month(disasters),
  end_date                    = plot_end_month(disasters),
  total_deaths                = plot_deaths(disasters),
  no_affected                 = plot_affected_ridge(disasters),
  # reconstruction_costs_000_us = plot_hist_log(disasters, "reconstruction_costs_000_us", "Reconstruction costs (thousand USD, log10)"),
  # total_damages_000_us        = plot_hist_log(disasters, "total_damages_000_us", "Total damages (thousand USD, log10)"),
  cpi                         = plot_cpi_time(disasters),
  event_mid_date              = plot_middate_rolling(disasters),
  event_duration_days         = plot_duration(disasters),
  # recon_usd_nominal           = plot_hist_log(disasters, "recon_usd_nominal", "Reconstruction nominal USD (log10)"),
  # damages_usd_nominal         = plot_hist_log(disasters, "damages_usd_nominal", "Damages nominal USD (log10)"),
  # recon_usd_real              = plot_hist_log(disasters, "recon_usd_real", "Reconstruction real USD (log10)"),
  # damages_usd_real            = plot_hist_log(disasters, "damages_usd_real", "Damages real USD (log10)"),
  fatality_rate               = plot_rate(disasters),
  severity_index              = plot_severity_heat(disasters)
)

# ---- 3) TWEETS: one specialized plot per column ----

# sampling for speed/sanity with 15.8M rows
TWEETS_SAMPLE_N <- 5e5
tweets_smpl <- if (nrow(tweets) > TWEETS_SAMPLE_N) dplyr::slice_sample(tweets, n = TWEETS_SAMPLE_N) else tweets

plot_created_at <- function(df) {
  df |>
    mutate(day = as.Date(created_at)) |>
    count(day) |>
    ggplot(aes(day, n)) +
    geom_line(color = gp_cols["blue"]) +
    labs(title = "Tweets per day (sample)", x = NULL, y = "Tweets") +
    theme_grand_budapest()
}



plot_id_qc <- function(df) {
  df |>
    mutate(day = as.Date(created_at)) |>
    slice_sample(n = min(2e5, n())) |>
    ggplot(aes(x = day, y = id)) +
    geom_point(alpha = .08, color = gp_cols["lavender"], size = .6) +
    labs(title = "QC: ID vs time (sample)", x = NULL, y = "Tweet ID") +
    theme_grand_budapest()
}
plot_lng_hist <- function(df) {
  df |> filter(between(lng, -180, 180), !is.na(lng)) |>
    ggplot(aes(lng)) +
    geom_histogram(bins = 100, fill = gp_cols["lavender"], color = gp_bg) +
    labs(title = "Longitude of tweets", x = "Longitude", subtitle = badge_missing(df$lng)) +
    theme_grand_budapest()
}
plot_lat_hist <- function(df) {
  df |> filter(between(lat, -90, 90), !is.na(lat)) |>
    ggplot(aes(lat)) +
    geom_histogram(bins = 100, fill = gp_cols["lavender"], color = gp_bg) +
    labs(title = "Latitude of tweets", x = "Latitude", subtitle = badge_missing(df$lat)) +
    theme_grand_budapest()
}
plot_topic_bar <- function(df) {
  df |>
    count(topic, sort = TRUE) |>
    mutate(topic = fct_reorder(topic, n)) |>
    ggplot(aes(n, topic, fill = topic)) +
    geom_col(show.legend = FALSE) + scale_gp_d_fill() +
    labs(title = "Topics", x = "Tweets", y = NULL) +
    theme_grand_budapest()
}
plot_sentiment_density <- function(df) {
  df |>
    mutate(sentiment_num2 = suppressWarnings(as.numeric(as.character(sentiment)))) |>
    filter(!is.na(sentiment_num2)) |>
    ggplot(aes(sentiment_num2)) +
    geom_density(fill = gp_cols["pink"], alpha = .9) +
    labs(title = "Sentiment (coerced numeric) density", x = "Sentiment", subtitle = badge_missing(df$sentiment)) +
    theme_grand_budapest()
}
plot_stance_bar <- function(df) {
  df |>
    count(stance) |>
    mutate(p = n/sum(n), stance = fct_reorder(stance, p)) |>
    ggplot(aes(p, stance, fill = stance)) +
    geom_col(show.legend = FALSE) + scale_gp_d_fill() +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(title = "Stance proportions", x = "Share", y = NULL) +
    theme_grand_budapest()
}
plot_gender_bar <- function(df) {
  df |>
    count(gender) |>
    mutate(p = n/sum(n), gender = fct_reorder(gender, p)) |>
    ggplot(aes(p, gender, fill = gender)) +
    geom_col(show.legend = FALSE) + scale_gp_d_fill() +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(title = "Gender proportions", x = "Share", y = NULL) +
    theme_grand_budapest()
}
plot_temp_hist <- function(df) {
  df |>
    filter(!is.na(temperature_avg)) |>
    ggplot(aes(temperature_avg)) +
    geom_histogram(bins = 50, fill = gp_cols["lavender"], color = gp_bg) +
    geom_density(linewidth = .8, color = gp_cols["blue"]) +
    labs(title = "Temperature (tweet context)", x = "°C (avg)", subtitle = badge_missing(df$temperature_avg)) +
    theme_grand_budapest()
}
plot_aggr_hist <- function(df) {
  df |>
    ggplot(aes(aggressiveness)) +
    geom_histogram(bins = 40, fill = gp_cols["lavender"], color = gp_bg, na.rm = TRUE) +
    labs(title = "Aggressiveness score", x = "Score", subtitle = badge_missing(df$aggressiveness)) +
    theme_grand_budapest()
}
plot_tweet_date <- function(df) {
  df |>
    count(tweet_date) |>
    ggplot(aes(tweet_date, n)) +
    geom_line(color = gp_cols["coral"]) +
    labs(title = "Tweets per day (date column)", x = NULL, y = "Tweets") +
    theme_grand_budapest()
}
plot_sentiment_num <- function(df) {
  ggplot(df, aes(sentiment_num)) +
    geom_histogram(bins = 40, fill = gp_cols["lavender"], color = gp_bg, na.rm = TRUE) +
    labs(title = "Sentiment (numeric) histogram", x = "Sentiment", subtitle = badge_missing(df$sentiment_num)) +
    theme_grand_budapest()
}

plot_created_at(tweets_smpl)
plot_id_qc(tweets_smpl)
plot_lng_hist(tweets_smpl)
plot_lat_hist(tweets_smpl)
plot_topic_bar(tweets_smpl)
plot_sentiment_density(tweets_smpl)
plot_stance_bar(tweets_smpl)
plot_gender_bar(tweets_smpl)
plot_temp_hist(tweets_smpl)
plot_aggr_hist(tweets_smpl)
plot_tweet_date(tweets_smpl)
plot_sentiment_num(tweets_smpl)

plots_tweets <- list(
  created_at      = plot_created_at(tweets_smpl),
  id              = plot_id_qc(tweets_smpl),
  lng             = plot_lng_hist(tweets_smpl),
  lat             = plot_lat_hist(tweets_smpl),
  topic           = plot_topic_bar(tweets_smpl),
  sentiment       = plot_sentiment_density(tweets_smpl),
  stance          = plot_stance_bar(tweets_smpl),
  gender          = plot_gender_bar(tweets_smpl),
  temperature_avg = plot_temp_hist(tweets_smpl),
  aggressiveness  = plot_aggr_hist(tweets_smpl),
  tweet_date      = plot_tweet_date(tweets_smpl),
  sentiment_num   = plot_sentiment_num(tweets_smpl)
)

# ---- 4) How to render or save ----
# Example: print a few key plots
# print(plots_disasters$country)
# print(plots_disasters$severity_index)
# print(plots_disasters$total_deaths)
# print(plots_tweets$topic)
# print(plots_tweets$created_at)

# Example: save all to a folder
# dir.create("grand_budapest_figs", showWarnings = FALSE)
# iw <- 9; ih <- 6
# purrr::iwalk(plots_disasters, ~ggsave(filename = file.path("grand_budapest_figs", paste0("disasters_", .y, ".png")),
#                                       plot = .x, width = iw, height = ih, dpi = 180, bg = "white"))
# purrr::iwalk(plots_tweets, ~ggsave(filename = file.path("grand_budapest_figs", paste0("tweets_", .y, ".png")),
#                                    plot = .x, width = iw, height = ih, dpi = 180, bg = "white"))
