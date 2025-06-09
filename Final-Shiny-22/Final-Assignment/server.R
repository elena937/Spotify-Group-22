library(shiny)
library(ggplot2)
library(glmnet)
library(dplyr)
library(caret)

spotify_data <- read.csv("Spotify-2000.csv")
spotify_data <- spotify_data %>%
  rename(
    BPM = `Beats.Per.Minute..BPM.`,
    Loudness = `Loudness..dB.`,
    Genre = `Top.Genre`,
    Length = `Length..Duration.`
  ) %>%
  mutate(
    BPM = as.numeric(BPM),
    Energy = as.numeric(Energy),
    Danceability = as.numeric(Danceability),
    Loudness = as.numeric(Loudness),
    Valence = as.numeric(Valence),
    Length = as.numeric(Length),
    Acousticness = as.numeric(Acousticness),
    Speechiness = as.numeric(Speechiness),
    Liveness = as.numeric(Liveness),
    Popularity = as.numeric(Popularity),
    Year = as.numeric(Year)
  )
spotify_data <- na.omit(spotify_data)

shinyServer(function(input, output) {
  
  data_split <- reactive({
    set.seed(123)
    train_idx <- sample(1:nrow(spotify_data), size = floor(input$train_ratio * nrow(spotify_data)))
    list(train = spotify_data[train_idx, ],
         test = spotify_data[-train_idx, ])
  })
  
  fit_model <- reactive({
    train_data <- data_split()$train
    test_data <- data_split()$test
    
    x_train <- as.matrix(train_data[, input$predictors])
    x_test <- as.matrix(test_data[, input$predictors])
    y_train <- train_data$Popularity
    y_test <- test_data$Popularity
    
    if (input$model_type == "lm") {
      model <- lm(Popularity ~ ., data = train_data[, c(input$predictors, "Popularity")])
      preds <- predict(model, newdata = test_data)
    } else {
      alpha_val <- ifelse(input$model_type == "ridge", 0, 1)
      cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha_val)
      preds <- predict(cv_fit, s = "lambda.min", newx = x_test)
      model <- cv_fit
    }
    
    mse <- mean((preds - y_test)^2)
    list(model = model, preds = preds, mse = mse)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(spotify_data, aes_string(x = input$predictors[1], y = "Popularity")) +
      geom_point(alpha = 0.5) +
      theme_minimal() +
      labs(title = "Popularity vs Selected Predictor")
  })
  
  output$msePlot <- renderPlot({
    vars <- c("Year", "Energy", "Loudness", "Liveness", "Length", "Acousticness", "Speechiness")
    mse_values <- numeric()
    
    for (k in 1:length(vars)) {
      combs <- combn(vars, k, simplify = FALSE)
      mse_k <- sapply(combs, function(cols) {
        set.seed(123)
        train_idx <- sample(1:nrow(spotify_data), size = floor(input$train_ratio * nrow(spotify_data)))
        train <- spotify_data[train_idx, ]
        test <- spotify_data[-train_idx, ]
        
        model <- lm(Popularity ~ ., data = train[, c(cols, "Popularity")])
        preds <- predict(model, newdata = test)
        mean((preds - test$Popularity)^2)
      })
      mse_values[k] <- min(mse_k)
    }
    
    df <- data.frame(
      NumPredictors = 1:length(vars),
      MSE = mse_values
    )
    
    ggplot(df, aes(x = NumPredictors, y = MSE)) +
      geom_line() + geom_point() +
      theme_minimal() +
      labs(title = "Out-of-sample MSE vs. number of predictor variables",
           x = "Number of predictor variables",
           y = "Out-of-sample MSE")
  })
  
  output$model_summary <- renderPrint({
    summary <- fit_model()
    if (input$model_type == "lm") {
      summary(summary$model)
    } else {
      summary$model
    }
  })
  
  output$mse_text <- renderText({
    paste("The out-of-sample MSE of the model is:", round(fit_model()$mse, 2))
  })
})
