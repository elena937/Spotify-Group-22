library(shiny)

shinyUI(fluidPage(
  titlePanel("Spotify Popularity Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Predicting the Popularity of Spotify Songs"),
      
      checkboxGroupInput("predictors",
                         "Selecting Predictive Variables:",
                         choices = c("Year", "Energy", "Loudness", "Liveness",
                                     "Length", "Acousticness", "Speechiness"),
                         selected = c("Year", "Energy", "Loudness")),
      
      selectInput("model_type", "Select model type:",
                  choices = c("Linear Regression" = "lm",
                              "Ridge Regression" = "ridge",
                              "Lasso Regression" = "lasso")),
      
      sliderInput("train_ratio", "training set ratio:",
                  min = 0.5, max = 0.9, value = 0.7)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data visualization", 
                 plotOutput("scatterPlot")),
        tabPanel("Model performance curve", 
                 plotOutput("msePlot")),
        tabPanel("Model output", 
                 verbatimTextOutput("model_summary"),
                 textOutput("mse_text"))
      )
    )
  )
))

