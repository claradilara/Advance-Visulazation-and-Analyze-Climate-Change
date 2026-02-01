###########################################
# Climate Sentiment & Disaster Dashboard
###########################################

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(leaflet)
library(sf)
library(janitor)
library(scales)
library(forcats)
library(rnaturalearth)
library(rnaturalearthdata)

# ============================================
# GRAND BUDAPEST PALETTE
# ============================================



sentiment_colors <- c(
  negative = "#5B1A18",  # burgundy
  neutral  = "#C6CDF7",  # lavender
  positive = "#7294D4"   # blue
)

gp_cols <- c(
  negative = "#5B1A18",
  neutral  = "#C6CDF7",
  positive = "#7294D4",
  coral    = "#F7A07A",
  pink     = "#F2C1CC",
  cream    = "#FAF4E8"
)

theme_grand_budapest <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = gp_cols["cream"], color = NA),
      panel.background = element_rect(fill = gp_cols["cream"], color = NA),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(color = "black"),
      axis.title       = element_text(color = "black"),
      plot.title       = element_text(face = "bold", size = 15),
      legend.position  = "none"
    )
}

# ============================================
# LOAD DATA
# ============================================

tweets_raw <- read_csv(
  "The Climate Change Twitter Dataset.csv",
  n_max = 10000,
  show_col_types = FALSE
) %>% clean_names()

disasters <- read_csv(
  "disasters.csv",
  show_col_types = FALSE
) %>% clean_names()

# ============================================
# DATA PREP
# ============================================

tweets <- tweets_raw %>%
  mutate(
    created_at = ymd_hms(created_at),
    tweet_date = as.Date(created_at),
    year  = year(tweet_date),
    month = format(tweet_date, "%Y-%m"),
    lat = as.numeric(lat),
    lng = as.numeric(lng),
    sentiment_polarity = as.numeric(sentiment),
    sentiment_cat = case_when(
      sentiment_polarity >  0.05 ~ "positive",
      sentiment_polarity < -0.05 ~ "negative",
      TRUE ~ "neutral"
    ) %>% factor(levels = c("negative","neutral","positive"))
  )

# ============================================
# UI
# ============================================

ui <- navbarPage(
  title = "Climate & Sentiment Dashboard",
  theme = shinytheme("flatly"),
  
  # -------- DISASTER EXPLORER --------
  tabPanel(
    "Disaster Explorer",
    fluidRow(
      column(
        3,
        selectInput(
          "dis_type",
          "Select Disaster Type:",
          choices = unique(disasters$disaster_type),
          selected = unique(disasters$disaster_type)[1]
        ),
        sliderInput(
          "year_range",
          "Year Range:",
          min = 1980,
          max = 2022,
          value = c(2000, 2010),
          sep = ""
        )
      ),
      column(9, plotOutput("dis_deaths_damages", height = "450px"))
    )
  ),
  
  # -------- TWEET EXPLORER --------
  tabPanel(
    "Tweet Explorer",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "tweet_month",
          "Select Month:",
          choices = sort(unique(tweets$month)),
          selected = sort(unique(tweets$month))[1]
        ),
        selectInput(
          "topic_sel",
          "Select Topic:",
          choices = sort(unique(tweets$topic)),
          selected = sort(unique(tweets$topic))[1]
        )
      ),
      mainPanel(
        plotOutput("sentiment_dist"),
        plotOutput("topic_freq")
      )
    )
  ),
  
  # -------- SENTIMENT TRENDS --------
  tabPanel(
    "Sentiment Trends",
    sidebarLayout(
      sidebarPanel(
        sliderInput("smooth_span", "Smoothing Level:", 0.1, 1, 0.5)
      ),
      mainPanel(
        plotOutput("daily_sentiment"),
        plotOutput("monthly_sentiment")
      )
    )
  ),
  
  # -------- MAP --------
  tabPanel(
    "Map Explorer",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "map_topic",
          "Filter by Topic:",
          choices = c("All", sort(unique(tweets$topic))),
          selected = "All"
        )
      ),
      mainPanel(
        leafletOutput("sent_map", height = "700px")
      )
    )
  )
  
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {
  
  # -------- DISASTER EXPLORER --------
  filtered_dis <- reactive({
    disasters %>%
      filter(disaster_type == input$dis_type) %>%
      mutate(year = year(start_date)) %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
  })
  
  output$dis_deaths_damages <- renderPlot({
    
    yearly <- filtered_dis() %>%
      group_by(year) %>%
      summarise(
        damages = sum(total_damages_000_us, na.rm = TRUE),
        deaths  = sum(total_deaths, na.rm = TRUE),
        .groups = "drop"
      )
    
    scale_factor <- max(yearly$damages) / max(yearly$deaths)
    
    ggplot(yearly, aes(factor(year))) +
      geom_col(aes(y = damages), fill = gp_cols["coral"]) +
      geom_line(aes(y = deaths * scale_factor, group = 1),
                color = gp_cols["negative"]) +
      geom_point(aes(y = deaths * scale_factor),
                 color = gp_cols["negative"]) +
      scale_y_continuous(
        name = "Damages (000 USD)",
        sec.axis = sec_axis(~./scale_factor, name = "Deaths")
      ) +
      labs(
        title = "Yearly Disaster Damages and Deaths",
        subtitle = input$dis_type,
        x = "Year"
      ) +
      theme_grand_budapest()
  })
  
  # -------- SENTIMENT BARS (TIME) --------
  output$sentiment_dist <- renderPlot({
    
    tweets %>%
      filter(month == input$tweet_month) %>%
      count(sentiment_cat) %>%
      complete(sentiment_cat, fill = list(n = 0)) %>%
      ggplot(aes(sentiment_cat, n, fill = sentiment_cat)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = gp_cols) +
      labs(
        title = "Monthly Sentiment Distribution",
        subtitle = input$tweet_month,
        x = NULL,
        y = "Tweet Count"
      ) +
      theme_grand_budapest()
  })
  
  # -------- SENTIMENT BARS (TIME + TOPIC) --------
  output$topic_freq <- renderPlot({
    
    tweets %>%
      filter(month == input$tweet_month,
             topic == input$topic_sel) %>%
      count(sentiment_cat) %>%
      complete(sentiment_cat, fill = list(n = 0)) %>%
      ggplot(aes(sentiment_cat, n, fill = sentiment_cat)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = gp_cols) +
      labs(
        title = "Sentiment by Topic and Time",
        subtitle = paste(input$topic_sel, "–", input$tweet_month),
        x = NULL,
        y = "Tweet Count"
      ) +
      theme_grand_budapest()
  })
  
  # -------- SENTIMENT TRENDS --------
  output$daily_sentiment <- renderPlot({
    
    tweets %>%
      group_by(tweet_date) %>%
      summarise(mean_sent = mean(sentiment_polarity, na.rm = TRUE)) %>%
      ggplot(aes(tweet_date, mean_sent)) +
      geom_line(alpha = 0.3) +
      geom_smooth(span = input$smooth_span,
                  color = gp_cols["coral"],
                  fill  = gp_cols["pink"]) +
      labs(title = "Daily Mean Sentiment") +
      theme_grand_budapest()
  })
  
  output$monthly_sentiment <- renderPlot({
    
    tweets %>%
      group_by(month) %>%
      summarise(mean_sent = mean(sentiment_polarity, na.rm = TRUE)) %>%
      ggplot(aes(month, mean_sent, group = 1)) +
      geom_line(color = gp_cols["positive"]) +
      geom_point(color = gp_cols["negative"]) +
      labs(title = "Monthly Mean Sentiment") +
      theme_grand_budapest()
  })
  
  # -------- MAP --------
  # output$sent_map <- renderLeaflet({
  #   
  #   map_data <- tweets %>%
  #     filter(!is.na(lat), !is.na(lng))
  #   
  #   if (input$map_topic != "All") {
  #     map_data <- map_data %>%
  #       filter(topic == input$map_topic)
  #   }
  #   
  #   leaflet(map_data) %>%
  #     addTiles() %>%
  #     addCircleMarkers(
  #       lng = ~lng,
  #       lat = ~lat,
  #       
  #       # SIZE reacts well to zoom
  #       radius = ~pmax(4, abs(sentiment_polarity) * 7),
  #       
  #       # SENTIMENT COLOR (CORRECT WAY)
  #       color     = ~gp_cols[sentiment_cat],   # stroke
  #       fillColor = ~gp_cols[sentiment_cat],   # fill
  #       fillOpacity = 0.8,
  #       opacity = 1,
  #       weight = 2,        # thicker border = visible when zoomed
  #       
  #       popup = ~paste0(
  #         "<b>Topic:</b> ", topic, "<br>",
  #         "<b>Sentiment:</b> ", sentiment_cat, "<br>",
  #         "<b>Polarity:</b> ", round(sentiment_polarity, 2), "<br>",
  #         "<b>Date:</b> ", tweet_date
  #       ),
  #       
  #       clusterOptions = markerClusterOptions()
  #     ) %>%
  #     addLegend(
  #       position = "bottomright",
  #       colors = gp_cols[c("negative","neutral","positive")],
  #       labels = c("Negative", "Neutral", "Positive"),
  #       title = "Sentiment",
  #       opacity = 1
  #     )
  # })
  # 
  
  
  output$sent_map <- renderLeaflet({
    
    map_data <- tweets %>%
      filter(!is.na(lat), !is.na(lng))
    
    if (input$map_topic != "All") {
      map_data <- map_data %>%
        filter(topic == input$map_topic)
    }
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        
        # SIZE (visible at all zoom levels)
        radius = ~pmax(4, abs(sentiment_polarity) * 7),
        
        # SENTIMENT COLOR
        color     = ~gp_cols[sentiment_cat],   # border
        fillColor = ~gp_cols[sentiment_cat],   # fill
        fillOpacity = 0.85,
        opacity = 1,
        weight = 2,
        
        # ✅ HOVER LABEL (THIS IS THE RIGHT WAY)
        label = ~paste0(
          "Topic: ", topic, "<br>",
          "Sentiment: ", sentiment_cat
        ),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px",
          style = list(
            "background-color" = "white",
            "border-color" = "#333333",
            "border-radius" = "4px",
            "padding" = "4px"
          )
        ),
        
        # CLICK POPUP (DETAILED)
        popup = ~paste0(
          "<b>Topic:</b> ", topic, "<br>",
          "<b>Sentiment:</b> ", sentiment_cat, "<br>",
          "<b>Polarity:</b> ", round(sentiment_polarity, 2), "<br>",
          "<b>Date:</b> ", tweet_date
        ),
        
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = "bottomright",
        colors = gp_cols[c("negative","neutral","positive")],
        labels = c("Negative", "Neutral", "Positive"),
        title = "Sentiment",
        opacity = 1
      )
  })
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
