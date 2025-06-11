library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(readr)
library(tidyverse)
library(bslib)

spotify_data <- read.csv("Spotify-2000.csv") %>%
  dplyr::rename(
    BPM = `Beats.Per.Minute..BPM.`,
    Loudness = `Loudness..dB.`,
    Genre = `Top.Genre`,
    Length = `Length..Duration.`
  ) %>%
  dplyr::mutate(
    Genre_Category = categorize_genres(Genre),
    dplyr::across(
      c(BPM, Energy, Danceability, Loudness, Valence, Length, 
        Acousticness, Speechiness, Liveness, Popularity, Year), as.numeric
    )
  ) %>%
  dplyr::select(-Index) %>%
  na.omit()

numeric_vars <- names(spotify_data)[sapply(spotify_data, is.numeric)]
model_vars <- setdiff(numeric_vars, "Popularity")

shinyUI(fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",  
    primary = "#1DB954",   # Spotify Green
    base_font = font_google("Open Sans")
  ),
  titlePanel(
    div(
      style = "text-align: center; margin-top: 10px; margin-bottom: 20px;",
      img(src = "LOGO.png", height = "100px"),
      h1("Spotify Explorer", 
         style = "font-weight: bold; font-size: 36px; color: #1DB954; margin-top: 20px;"),
      p("Explore trends and predict popularity in music from 1956–2019", 
        style = "font-size: 18px; color: #666;")
    ),
  ),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Initial Visualization'",
        h4("🎵 Line Plot Controls"),
        wellPanel(
          sliderInput("line_year_range", "Select Year Range:", 
                      min = 1956, max = 2019,
                      value = c(2000, 2019), step = 1),
          helpText("Use this slider to filter songs by release year.")
        ),
        tags$hr(),
        h4("📊 Histogram Controls"),
        wellPanel(
          selectInput("hist_var", "Select variable to plot histogram:", 
                      choices = numeric_vars, selected = "Popularity"),
          helpText("Choose a numeric variable to visualize its distribution.")
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Correlation Heatmap'",
        h4("🔗 Heatmap Controls"),
        wellPanel(
          checkboxGroupInput("corr_vars", "Select numeric variables:",
                             choices = numeric_vars,
                             selected = numeric_vars),
          selectInput("heatmap_palette", "Heatmap Color Palette:",
                      choices = c("Green/Purple", "Blue/Red", "Pink/Orange"),
                      selected = "Green/Purple"),
          checkboxGroupInput("heatmap_genres", "Select Genres for Heatmap:",
                             choices = unique(spotify_data$Genre_Category),
                             selected = unique(spotify_data$Genre_Category)),
          helpText("Subset data by genres and numeric variables.")
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Best Subset Selection' || input.tabs == 'Lasso Method'",
        h4("⚙️ Train/Test Split Controls"),
        wellPanel(
          sliderInput("train_ratio", "Training Set Proportion:", min = 0.3, max = 0.8, value = 0.5, step = 0.05),
          sliderInput("test_ratio", "Testing Set Proportion:", min = 0.1, max = 0.5, value = 0.2, step = 0.05),
          checkboxGroupInput("model_vars", "Select numeric variables for modeling:",
                             choices = model_vars,
                             selected = model_vars),
          helpText("Adjust the train/test split and select model variables.")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Initial Visualization",
                 div(style = "text-align:center", h3("📈 Popularity over Year")),
                 plotOutput("line_plot", height = "400px", width = "100%"),
                 tags$hr(),
                 div(style = "text-align:center", h3("📊 Histogram")),
                 plotOutput("hist_plot", height = "400px", width = "100%")
        ),
        
        tabPanel("Correlation Heatmap",
                 div(style = "text-align:center", h3("📉 Correlation Heatmap")),
                 plotOutput("corr_plot", height = "500px", width = "100%")
        ),
        
        tabPanel("Best Subset Selection",
                 div(style = "text-align:center", h3("📊 Popularity Distribution by Data Split")),
                 plotOutput("split_plot", height = "350px"),
                 tags$hr(),
                 h3("📉 Validation MSE by Model Size"),
                 plotOutput("cv_mse_plot", height = "350px"),
                 verbatimTextOutput("best_model_info"),
                 tags$hr(),
                 h3("📌 Predicted vs Actual (Test Set)"),
                 plotOutput("pred_vs_actual_plot", height = "350px"),
                 tags$hr(),
                 h3("📋 Final Model Coefficients and T-Statistics"),
                 textOutput("summary_text_ols"),
                 tableOutput("ols_summary")
        ),
        
        
        tabPanel("Lasso Method",
                 div(style = "text-align:center", h3("🔍 Cross-Validated Lasso Plot")),
                 plotOutput("cv_lasso_plot", height = "350px"),
                 tags$hr(),
                 h3("📌 Predicted vs Actual (Test Set)"),
                 plotOutput("lasso_pred_vs_actual", height = "350px"),
                 verbatimTextOutput("lasso_model_info"),
                 tags$hr(),
                 h3("📋 Final Model Coefficients and T-Statistics"),
                 textOutput("lasso_summary_text"),
                 tableOutput("lasso_summary")
        )
      )
    )
  )
))