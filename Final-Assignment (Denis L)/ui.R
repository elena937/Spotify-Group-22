library(shiny)

spotify_data <- read.csv("Spotify-2000.csv") %>%
  dplyr::rename(
    BPM = `Beats.Per.Minute..BPM.`,
    Loudness = `Loudness..dB.`,
    Genre = `Top.Genre`,
    Length = `Length..Duration.`
  ) %>%
  dplyr::mutate(dplyr::across(
    c(BPM, Energy, Danceability, Loudness, Valence, Length, 
      Acousticness, Speechiness, Liveness, Popularity, Year), as.numeric
  )) %>%
  dplyr::select(-Index) %>%
  na.omit()

numeric_vars <- names(spotify_data)[sapply(spotify_data, is.numeric)]

shinyUI(fluidPage(
  titlePanel("Spotify Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Histogram Controls"),
      selectInput("hist_var", "Select variable to plot histogram:", 
                  choices = numeric_vars, selected = "Popularity"),
      
      hr(),
      h4("Variable choice for model selection"),
      checkboxGroupInput("corr_vars", "Select numeric variables:",
                         choices = numeric_vars,
                         selected = numeric_vars),
      
      hr(),
      h4("Train/Test Split Controls"),
      sliderInput("train_ratio", "Training Set Proportion:", min = 0.3, max = 0.8, value = 0.5, step = 0.05),
      sliderInput("test_ratio", "Testing Set Proportion:", min = 0.1, max = 0.5, value = 0.2, step = 0.05)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Initial Visualization",
                 h3("1. Histogram"),
                 plotOutput("hist_plot"),
                 hr(),
                 h3("2. Correlation Heatmap"),
                 plotOutput("corr_plot")
        ),
        tabPanel("Best Model Search (Holdout)",
                 h3("1. Popularity Distribution by Data Split"),
                 plotOutput("split_plot"),
                 hr(),
                 h3("2. Validation MSE by Model Size"),
                 plotOutput("cv_mse_plot"),
                 verbatimTextOutput("best_model_info"),
                 hr(),
                 h3("3. Predicted vs Actual (Test Set)"),
                 plotOutput("pred_vs_actual_plot")
        )
      )
    )
  )
))