# ---- Packages ---------------------------------------------------------------
# install.packages(c("tidyverse","sf","rnaturalearth","rnaturalearthdata",
#                    "lubridate","scales","forcats","ggrepel","ggthemes",
#                    "patchwork","wesanderson","ggiraph","janitor"))

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(scales)
library(forcats)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(wesanderson)
library(ggiraph)
library(janitor)

# ---- 1.0) Load & prepare data -------------------------------------------------
disasters_raw <- readr::read_csv("C:/Users/JasmineJiang/Desktop/s3/5.a2 Advanced Visualisation in R/00.Project/archive/disasters.csv", show_col_types = FALSE) %>% clean_names()
tweets_raw    <- readr::read_csv("C:/Users/JasmineJiang/Desktop/s3/5.a2 Advanced Visualisation in R/00.Project/archive/The Climate Change Twitter Dataset.csv",    show_col_types = FALSE) %>% clean_names()

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

# ---- 1.1) Grand Budapest Hotel palette + theme ----
# Discrete palette inspired by 'wesanderson' GrandBudapest1/2
gp_cols <- c(
  "pink"            = "#E6A0C4",
  "coral"           = "#FD6467",
  "gold"            = "#F1BB7B",
  "mustard"         = "#F2AD00",
  "burgundy"        = "#5B1A18",
  "lavender"        = "#C6CDF7",
  "blue"            = "#7294D4",
  "sand"            = "#D8A499",
  "mint_green"      = "#A2CCB8",
  "cream_yellow"    = "#FFEBB7",
  "violet"          = "#8872A0",
  "deep_emerald"    = "#004F43",
  "bright_orange"   = "#FF9933",
  "light_grey"      = "#CCCCCC",
  "dark_cyan"       = "#008C99"
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

# N.A proportion
not_na_prop <- function(x) mean(!is.na(x))
badge_missing <- function(x) {
  pct <- scales::percent(mean(is.na(x)))
  glue::glue("Missing: {pct}")
}


# ---- 2.1) Dataset Introduction ----
# Data preparation
# Assuming climate_data is your data frame with columns disaster_type and start_date
names(disasters)

disaster_counts <- disasters %>%
  mutate(start_date = ymd(start_date),              
         year = year(start_date)) %>%              
  filter(!is.na(disaster_type), !is.na(year)) %>%   
  count(year, disaster_type, name = "n")           

# Plotting:Disaster Types Across Years
year_totals <- disaster_counts %>%
  group_by(year) %>%
  summarise(total = sum(n), .groups = "drop")

ggplot(disaster_counts, aes(x = factor(year), y = n, fill = disaster_type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = year_totals, 
            aes(x = factor(year), y = total, label = total), 
            vjust = -0.3, 
            color = '#5B1A18', 
            size = 3.5,
            inherit.aes = FALSE) +
  scale_gp_d_fill() +
  labs(title = "Disaster Types Across Years",
       x = "Year",
       y = "Number of Disasters",
       fill = "Disaster Type") +
  theme_grand_budapest()+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())


# Plotting:Disaster By Country Across Years (Top 15 Countries)
country_counts <- disasters %>%
  mutate(start_date = ymd(start_date),
         year = year(start_date)) %>%
  filter(!is.na(country), !is.na(year)) %>%
  count(year, country, name = "n")


top15_countries <- country_counts %>%
  group_by(country) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 15) %>%
  pull(country)

country_counts_top15 <- country_counts %>%
  filter(country %in% top15_countries)


country_totals <- country_counts_top15 %>%
  group_by(year) %>%
  summarise(total = sum(n), .groups = "drop")


ggplot(country_counts_top15, aes(x = factor(year), y = n, fill = country)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = country_totals, 
            aes(x = factor(year), y = total, label = total), 
            vjust = -0.3, 
            color = '#5B1A18', 
            size = 3.5,
            inherit.aes = FALSE) +
  scale_gp_d_fill() +
  labs(title = "Disaster By Country Across Years (Top 15 Countries)",
       x = "Year",
       y = "Number of Disasters",
       fill = "Country") +
  theme_grand_budapest() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Summarise data by year
year_summary <- disasters %>%
  mutate(start_date = ymd(start_date),
         year = year(start_date)) %>%
  filter(!is.na(year), !is.na(total_damages_000_us), !is.na(total_deaths)) %>%
  group_by(year) %>%
  summarise(
    total_damages = sum(total_damages_000_us, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Plotting: Total Damages (bar) + Total Deaths (line)
# Prepare Data
yearly_summary <- disasters %>%
  mutate(start_date = ymd(start_date),
         year = year(start_date)) %>%
  filter(!is.na(year), !is.na(total_deaths), !is.na(total_damages_000_us)) %>%
  group_by(year) %>%
  summarise(
    total_damages = sum(total_damages_000_us, na.rm = TRUE),
    total_deaths = sum(total_deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Scaling factor for dual axis
max_damage <- max(yearly_summary$total_damages)
max_deaths <- max(yearly_summary$total_deaths)
scale_factor <- max_damage / max_deaths

# Plotting:Yearly Disaster Damages and Deaths
ggplot(yearly_summary, aes(x = factor(year))) +
  # Bar plot: Damages
  geom_bar(aes(y = total_damages), stat = "identity", fill = "#F7A07A") +
  
  # Line plot: Deaths (rescaled)
  geom_line(aes(y = total_deaths * scale_factor, group = 1), color = "#5B1A18", size = 1) +
  geom_point(aes(y = total_deaths * scale_factor), color = "#5B1A18", size = 2) +
  
  # Data labels for bar plot
  geom_text(aes(y = total_damages, label = comma(total_damages)), 
            vjust = -0.5,
            color = "black", size = 3) +
  
  # Data labels for line plot
  geom_text(aes(y = total_deaths * scale_factor, label = comma(total_deaths)), 
            vjust = -0.5,
            color = "#5B1A18", size = 3) +
  
  # Axis labels and scales
  scale_y_continuous(
    name = "Total Damages (thousand USD)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Total Deaths")
  ) +
  
  labs(title = "Yearly Disaster Damages and Deaths",
       x = "Year") +
  
  theme_grand_budapest() +
  theme(
    axis.title.y = element_text(color = "#F7A07A", size = 12),
    axis.title.y.right = element_text(color = "#5B1A18", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Plotting:Average Yearly Disaster Damages and Deaths by Typ
average_summary <- disasters %>%
  group_by(disaster_type) %>%
  summarise(
    avg_damages = mean(total_damages_000_us, na.rm = TRUE),
    avg_deaths = mean(total_deaths, na.rm = TRUE)
  ) %>%
  ungroup()


ggplot(average_summary, aes(x = reorder(disaster_type, avg_damages))) +
  # Bar plot: Average Damages
  geom_bar(aes(y = avg_damages), stat = "identity", fill = "#F7A07A") +
  
  # Line plot: Average Deaths (rescaled)
  geom_line(aes(y = avg_deaths * scale_factor, group = 1), color = "#5B1A18", size = 1) +
  geom_point(aes(y = avg_deaths * scale_factor), color = "#5B1A18", size = 2) +
  
  # Data labels for bar plot
  geom_text(aes(y = avg_damages, label = comma(avg_damages)), 
            vjust = -0.5,  # 调整标签位置，使其位于柱子上方
            color = "black", size = 3) +
  
  # Data labels for line plot
  geom_text(aes(y = avg_deaths * scale_factor, label = comma(avg_deaths)), 
            vjust = -0.5,  # 调整标签位置，使其位于点的下方
            color = '#5B1A18', size = 3) +
  
  # Axis labels and scales
  scale_y_continuous(
    name = "Average Damages (thousand USD)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average Deaths")
  ) +
  
  labs(title = "Average Yearly Disaster Damages and Deaths by Type",
       x = "Disaster Type") +
  
  theme_grand_budapest() +
  theme(
    axis.title.y = element_text(color = "#F7A07A", size = 10),
    axis.title.y.right = element_text(color = "#5B1A18", size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1,size = 10, face = "bold")  # 旋转 x 轴标签，避免重叠
  )


# Plotting:Disaster types

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

#Plotting: Disaster events by country (choropleth)
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
























