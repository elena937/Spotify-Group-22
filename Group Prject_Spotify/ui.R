library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(readr)
library(tidyverse)
library(shinythemes)

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
  theme = shinytheme("cosmo"),
  titlePanel("Spotify Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Initial Visualization'",
        h4("Line Plot Controls"),
        sliderInput("line_year_range", "Select Year Range:", min = 1956, max = 2019,
                    value = c(2000, 2019), step = 1),
        
        hr(),
        h4("Histogram Controls"),
        selectInput("hist_var", "Select variable to plot histogram:", 
                    choices = numeric_vars, selected = "Popularity")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Correlation Heatmap'",
        h4("Heatmap Controls"),
        checkboxGroupInput("corr_vars", "Select numeric variables:",
                           choices = numeric_vars,
                           selected = numeric_vars),
        
        selectInput("heatmap_palette", "Heatmap Color Palette:",
                    choices = c("Green/Purple", "Blue/Red", "Pink/Orange"),
                    selected = "Green/Purple"),
        
        checkboxGroupInput("heatmap_genres", "Select Genres for Heatmap:",
                           choices = unique(spotify_data$Genre_Category),
                           selected = unique(spotify_data$Genre_Category))
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Best Subset Selection' || input.tabs == 'Lasso Method'",
        h4("Train/Test Split Controls"),
        sliderInput("train_ratio", "Training Set Proportion:", min = 0.3, max = 0.8, value = 0.5, step = 0.05),
        sliderInput("test_ratio", "Testing Set Proportion:", min = 0.1, max = 0.5, value = 0.2, step = 0.05),
        
        h4("Model Variables"),
        checkboxGroupInput("model_vars", "Select numeric variables for modeling:",
                           choices = model_vars,
                           selected = model_vars)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Initial Visualization",
                 h3("Popularity over Year"),
                 plotOutput("line_plot"),
                 hr(),
                 h3("Histogram"),
                 plotOutput("hist_plot")
        ),
        tabPanel("Correlation Heatmap",
                 h3("Correlation Heatmap"),
                 plotOutput("corr_plot")
        ),
        tabPanel("Best Subset Selection",
                 h3("Popularity Distribution by Data Split"),
                 plotOutput("split_plot"),
                 hr(),
                 h3("Validation MSE by Model Size"),
                 plotOutput("cv_mse_plot"),
                 verbatimTextOutput("best_model_info"),
                 hr(),
                 h3("Predicted vs Actual (Test Set)"),
                 plotOutput("pred_vs_actual_plot"),
                 hr(),
                 h3("Final Model Coefficients and T-Statistics"),
                 textOutput("summary_text_ols"),
                 tableOutput("ols_summary")
        ),
        tabPanel("Lasso Method",
                 h3("Cross-Validated Lasso Plot"),
                 plotOutput("cv_lasso_plot"),
                 hr(),
                 h3("Predicted vs Actual (Test Set)"),
                 plotOutput("lasso_pred_vs_actual"),
                 verbatimTextOutput("lasso_model_info"),
                 hr(),
                 h3("Final Model Coefficients and T-Statistics"),
                 textOutput("lasso_summary_text"),
                 tableOutput("lasso_summary")
        )
      )
    )
  )
))