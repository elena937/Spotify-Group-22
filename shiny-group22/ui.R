library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(readr)
library(tidyverse)
library(bslib)
library(shinyBS)
library(shinycssloaders)

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

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1DB954", # Spotify Green
    secondary = "#A4E3DA",
    font_scale = 1.2,
    base_font = "Montserrat, Arial, sans-serif"
  ),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600;700&display=swap"),
    tags$style(HTML("
      body, h1, h2, h3, h4, p { font-family: 'Montserrat', sans-serif; }
      body { background-color: #F5F5F5; }
      .btn-primary { background-color: #1DB954; border-color: #1DB954; }
      .btn-primary:hover { background-color: #1ED760; border-color: #1ED760; }
      .well { background-color: #F5F7FA; box-shadow: 0 4px 8px rgba(0,0,0,0.1); border-radius: 8px; }
      .nav-tabs > li > a { color: #191414; }
      .nav-tabs > li.active > a { background-color: #1DB954 !important; color: white !important; }
    "))
  ),

  
  titlePanel(
    div(
      style = "text-align: center; margin-top: 10px; margin-bottom: 20px;",
      img(src = "LOGO.png", height = "100px"),
      h1("Spotify Explorer", 
         style = "font-weight: bold; font-size: 36px; color: #1DB954; margin-top: 20px;"),
      p("Our application allows you to explore trends in Spotify song data(1956-2019) and apply statistical modeling methods to predict song popularity.", 
        style = "font-weight: bold; font-size: 18px; color: #191414;") #Spotify Black
    ),
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "padding: 15px; background-color: #FFFFFF; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); max-width: 300px;",
      conditionalPanel(
        condition = "input.tabs == 'Initial Visualization'",
        h4("Line Plot Controls"),
        card(
          card_body(
            sliderInput("line_year_range", "Select Year Range:", 
                        min = 1956, max = 2019,
                        value = c(2000, 2019), step = 1),
            bsTooltip("line_year_range", "Adjust the year range to filter songs.", placement = "right")
          )
        ),
        tags$hr(),
        h4("Histogram Controls"),
        wellPanel(
          selectInput("hist_var", "Select variable to plot histogram:", 
                      choices = numeric_vars, selected = "Popularity"),
          helpText("Choose a numeric variable to visualize its distribution.")
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Correlation Heatmap'",
        h4("Heatmap Controls"),
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
        h4("Train/Test Split Controls"),
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
                 div(style = "text-align:center", h3("Popularity over Year")),
                 plotOutput("line_plot", height = "400px", width = "100%"),
                 tags$hr(),
                 div(style = "text-align:center", h3("Histogram")),
                 plotOutput("hist_plot", height = "400px", width = "100%")
        ),
        
        tabPanel("Correlation Heatmap",
                 div(style = "text-align:center", h3("Correlation Heatmap")),
                 plotOutput("corr_plot", height = "500px", width = "100%")
        ),
        
        tabPanel("Best Subset Selection",
                 div(style = "text-align:center", h3("Popularity Distribution by Data Split")),
                 plotOutput("split_plot", height = "350px"),
                 p("We compare different feature selection methods in predicting Spotify song popularity.")
                 ,
                 
                 tags$hr(),
                 h3("Validation MSE by Model Size"),
                 verbatimTextOutput("model_description_text1"),
                 plotOutput("cv_mse_plot", height = "350px"),
                 verbatimTextOutput("best_model_info"),
                 tags$hr(),
                 h3("Predicted vs Actual (Test Set)"),
                 plotOutput("pred_vs_actual_plot", height = "350px"),
                 tags$hr(),
                 h3("Final Model Coefficients and T-Statistics"),
                 textOutput("summary_text_ols"),
                 tableOutput("ols_summary"),
                 
        ),
        
       
        tabPanel("Lasso Method",
                 div(style = "text-align:center", h3("Cross-Validated Lasso Plot")),
                 plotOutput("cv_lasso_plot", height = "350px"),
                 verbatimTextOutput("model_description_text2"),
                 p("Compare different feature selection methods in predicting Spotify song popularity.")
                 ,
                 tags$hr(),
                 h3("Predicted vs Actual (Test Set)"),
                 plotOutput("lasso_pred_vs_actual", height = "350px"),
                 verbatimTextOutput("lasso_model_info"),
                 tags$hr(),
                 h3("Final Model Coefficients and T-Statistics"),
                 textOutput("lasso_summary_text"),
                 tableOutput("lasso_summary"),
        ),
        
        tabPanel("About the App",
                 h3("Research Question"),
                 p("Which audio features best explain the difference in popularity of Spotify music from 1956 to 2019, based on the top 2000 tracks?"),
                 
                 h3("Motivation"),
                 p("In the age of streaming, music popularity is no longer accidental, but can be quantified and analyzed.The Spotify dataset(https://www.kaggle.com/datasets/iamsumat/spotify-top-2000s-mega-dataset) provides audio features including energy, danceability, positive emotions, and more.This study examines the 2,000 most popular songs between 1956 and 2019 to analyze which of these features best explain differences in song popularity and to better understand listener preferences in the context of Historical Changes."),
                 
                 h3("Dataset Overview"),
                 p("The dataset includes 2000 songs with features like BPM, Energy, Danceability, Loudness, Acousticness, and more. We removed missing values and grouped genres into broader categories for interpretability."),
                 
                 h3("Group Countribution"),
                 p("This project was completed in collaboration with Group 22, 
                 [Chu Li(1082477): Discuss with team members to complete the Shiny app together,
                 part1 step1-2, code after feedback,
                 Improving the Shiny app user interface]
                   Denis Lebedev (4826973):Discuss with team members to complete the Shiny app together, step 3 (code) + some cosmetic features for all sections,
                   Tinghui Xu (1715119): Discuss with team members to complete the Shiny app together,
                   step 1&2 previous examples and revises;the interpretation for the lasso,the comparison between the 2 linear methods and the Shiny App，
                   Eleni Spyrou(4515919): Discuss with team members to complete the Shiny app together,step 3&4 (descriptive interpretation), lasso model code.")
        ),
      )
    )
  )
)

