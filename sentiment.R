# ==============================
#  1. Libraries
# ==============================
library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(janitor)


# ==============================
#  2. Load SAMPLE of tweets
# ==============================
tweets <- tweets %>%
  filter(
    !is.na(tweet_date),
    tweet_date >= as.Date("2006-01-01"),
    tweet_date <= as.Date("2011-12-31")
  )


# ==============================
#  3. Grand Budapest Palette
# ==============================
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


# ==============================
#  4. Theme (your friend’s theme)
# ==============================
theme_grand_budapest <- function(base_size = 12, base_family = "serif") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background   = element_rect(fill = "#FFF1F5", color = NA),
      panel.background  = element_rect(fill = "#FFF1F5", color = NA),
      panel.grid.major  = element_line(color = "#EBDDE2", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.title        = element_text(color = "#5B1A18", face = "bold"),
      axis.text         = element_text(color = "#6B5E62"),
      plot.title        = element_text(color = "#5B1A18", face = "bold", size = base_size * 1.35),
      legend.position   = "top",
      legend.title      = element_text(color = "#5B1A18"),
      legend.text       = element_text(color = "#6B5E62")
    )
}


# ==============================
#  5. Fix Sentiment Columns
# ==============================
tweets <- tweets_raw %>%
  mutate(
    created_at = suppressWarnings(lubridate::ymd_hms(created_at, quiet = TRUE)),
    tweet_date = as.Date(created_at),
    
    lat  = as.numeric(lat),
    lng  = as.numeric(lng),
    
    temperature_avg = as.numeric(temperature_avg),
    aggressiveness  = suppressWarnings(as.numeric(aggressiveness)),
    
    # sentiment is numeric polarity
    sentiment_polarity = as.numeric(sentiment),
    
    # convert polarity to categorical labels
    sentiment_cat = case_when(
      sentiment_polarity >  0.05 ~ "positive",
      sentiment_polarity < -0.05 ~ "negative",
      TRUE                        ~ "neutral"
    ) %>% factor(levels = c("negative","neutral","positive")),
    
    sentiment_num = case_when(
      sentiment_cat == "negative" ~ -1,
      sentiment_cat == "neutral"  ~  0,
      sentiment_cat == "positive" ~  1
    ),
    
    stance = factor(stance),
    topic  = factor(topic),
    gender = factor(gender)
  )

library(dplyr)

# ==============================
#  6. Sentiment Distribution Plot (FIXED)
# ==============================

# Remove any previous fill scale (important!)
ggplot(tweets, aes(x = sentiment_cat)) +
  geom_bar(aes(fill = sentiment_cat), color = gp_cols["burgundy"]) +
  
  # FORCE override of ANY previous scale
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  
  scale_fill_manual(
    values = setNames(
      c(gp_cols["coral"], gp_cols["lavender"], gp_cols["blue"]),
      c("negative", "neutral", "positive")
    )
  ) +
  
  labs(
    title = "Sentiment Distribution",
    x = NULL,
    y = "Tweet Count"
  ) +
  
  # Apply theme LAST (so scale remains intact)
  theme_grand_budapest()



#by topic

topic_sentiment <- tweets %>%
  group_by(topic) %>%
  summarise(mean_sentiment = mean(sentiment_polarity, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  filter(!is.na(topic)) %>%
  arrange(mean_sentiment)

ggplot(topic_sentiment, aes(mean_sentiment, fct_reorder(topic, mean_sentiment))) +
  geom_col(aes(fill = mean_sentiment)) +
  scale_fill_gradientn(colors = c(gp_cols["lavender"], gp_cols["pink"], gp_cols["coral"])) +
  labs(title = "Mean sentiment by topic", x = "Mean polarity", y = NULL) +
  theme_grand_budapest()

#by time for trend analysis
# ==============================
# DAILY SENTIMENT (ALL YEARS)
# ==============================

sentiment_daily <- tweets %>%
  filter(!is.na(tweet_date)) %>%
  group_by(tweet_date) %>%
  summarise(
    mean_sentiment = mean(sentiment_polarity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(sentiment_daily, aes(tweet_date, mean_sentiment)) +
  geom_line(color = gp_cols["burgundy"], alpha = 0.7) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = gp_cols["coral"],
    linewidth = 0.9
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  labs(
    title = "Daily Mean Sentiment Over Time",
    x = NULL,
    y = "Mean polarity"
  ) +
  theme_grand_budapest()

# ==============================
# MONTHLY SENTIMENT (ALL YEARS)
# ==============================

sentiment_month <- tweets %>%
  filter(!is.na(created_at)) %>%
  mutate(month = floor_date(created_at, "month")) %>%
  group_by(month) %>%
  summarise(
    mean_sentiment = mean(sentiment_polarity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(sentiment_month, aes(month, mean_sentiment)) +
  geom_line(color = gp_cols["blue"], linewidth = 1) +
  geom_point(color = gp_cols["burgundy"], size = 1.5) +
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "1 year"
  ) +
  labs(
    title = "Monthly Mean Sentiment Over Time",
    x = NULL,
    y = "Mean polarity"
  ) +
  theme_grand_budapest()




#Sentiment Geospatial Map(s)

sentiment_palette <- c(
  gp_cols["lavender"],
  gp_cols["pink"],
  gp_cols["coral"],
  gp_cols["burgundy"]
)


tweets_geo <- tweets %>%
  filter(!is.na(lat), !is.na(lng))

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")


tweets_geo_sf <- tweets_geo %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = world, fill = gp_cols["sand"], color = gp_cols["burgundy"], linewidth = .3) +
  geom_sf(data = tweets_geo_sf, aes(color = sentiment_polarity), alpha = .6, size = 1) +
  scale_color_gradientn(colors = c(gp_cols["lavender"], gp_cols["pink"], gp_cols["coral"], gp_cols["burgundy"])) +
  labs(
    title = "Tweet-Level Sentiment Map",
    subtitle = "Each point is a geolocated tweet",
    color = "Sentiment"
  ) +
  theme_grand_budapest()





library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")


tweets_geo_sf <- tweets %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)



tweets_with_country <- st_join(
  tweets_geo_sf,
  world[, c("name", "iso_a3")],   # country name + 3-letter code
  join = st_within
)




sentiment_country <- tweets_with_country %>%
  st_drop_geometry() %>%     # remove spatial structure for summarizing
  group_by(iso_a3, name) %>%
  summarise(
    mean_sentiment = mean(sentiment_polarity, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )




ggplot(world %>% left_join(sentiment_country, by = c("iso_a3"))) +
  geom_sf(aes(fill = mean_sentiment), color = gp_cols["burgundy"], linewidth = .3) +
  scale_fill_gradientn(
    colors = c(gp_cols["lavender"], gp_cols["pink"], gp_cols["coral"], gp_cols["burgundy"]),
    na.value = gp_cols["sand"],
    name = "Mean\nSentiment"
  ) +
  labs(
    title = "Country-Level Sentiment Toward Climate Topics",
    subtitle = "Detected automatically from tweet coordinates"
  ) +
  theme_grand_budapest()








#by gender



tweets %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(mean_sentiment = mean(sentiment_polarity, na.rm = TRUE)) %>%
  ggplot(aes(x = gender, y = mean_sentiment, fill = gender)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = unname(gp_cols)) +
  labs(
    title = "Average Sentiment Polarity by Gender",
    x = "Gender",
    y = "Mean Polarity"
  ) +
  theme_grand_budapest()




#by gender aggresivness AND SEVERITY  maybe added................................

model <- lm(
  sentiment_polarity ~ temperature_avg + topic + gender,
  data = tweets
)

summary(model)



#ANIMATED TREND ANALYSIS


install.packages("gganimate")
install.packages("gifski")
install.packages("transformr")

# ==============================
# ANIMATED MONTHLY TREND (ALL YEARS)
# ==============================

library(gganimate)

anim_timeline <- ggplot(sentiment_month, aes(month, mean_sentiment)) +
  geom_line(color = gp_cols["coral"], linewidth = 1) +
  geom_point(color = gp_cols["burgundy"], size = 2) +
  labs(
    title = "Monthly Mean Tweet Sentiment",
    subtitle = "Month: {frame_along}",
    x = NULL,
    y = "Mean polarity"
  ) +
  theme_grand_budapest() +
  transition_reveal(month)

animate(
  anim_timeline,
  fps = 20,
  duration = 10,
  width = 900,
  height = 450
)



# 
# library(gganimate)
# 
# # DAILY sentiment (if monthly, replace sentiment_daily with sentiment_month)
# anim_timeline <- ggplot(sentiment_daily, aes(x = created_at, y = mean_sentiment)) +
#   geom_line(color = gp_cols["coral"], linewidth = 1) +
#   geom_point(color = gp_cols["burgundy"], size = 2) +
#   labs(
#     title = "Daily Mean Tweet Sentiment",
#     subtitle = "Date: {frame_along}",
#     x = "", y = "Mean polarity"
#   ) +
#   theme_grand_budapest() +
#   transition_reveal(created_at)
# 
# animate(anim_timeline, fps = 25, duration = 8, width = 900, height = 450)
# anim_save("sentiment_timeline.gif")



