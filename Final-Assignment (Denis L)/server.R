library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(readr)

source("generate_formulas.R")

spotify_data <- read.csv("Spotify-2000.csv") %>%
  rename(
    BPM = `Beats.Per.Minute..BPM.`,
    Loudness = `Loudness..dB.`,
    Genre = `Top.Genre`,
    Length = `Length..Duration.`
  ) %>%
  mutate(across(
    c(BPM, Energy, Danceability, Loudness, Valence, Length, 
      Acousticness, Speechiness, Liveness, Popularity, Year), as.numeric
  )) %>%
  select(-Index) %>%
  na.omit()

shinyServer(function(input, output) {
  
  # Shared data split
  data_split <- reactive({
    req(input$train_ratio + input$test_ratio <= 0.99)
    set.seed(123)
    
    train_index <- createDataPartition(spotify_data$Popularity, p = input$train_ratio, list = FALSE)
    df_train <- spotify_data[train_index, ]
    
    remaining <- spotify_data[-train_index, ]
    val_share <- 1 - input$train_ratio - input$test_ratio
    valid_index <- createDataPartition(remaining$Popularity, p = val_share / (val_share + input$test_ratio), list = FALSE)
    df_valid <- remaining[valid_index, ]
    df_test  <- remaining[-valid_index, ]
    
    list(train = df_train, valid = df_valid, test = df_test)
  })
  
  output$hist_plot <- renderPlot({
    ggplot(spotify_data, aes_string(x = input$hist_var)) +
      geom_histogram(binwidth = 5, fill = "darkgreen", color = "white", alpha = 0.8) +
      labs(title = paste("Distribution of", input$hist_var)) +
      theme_minimal()
  })
  
  output$corr_plot <- renderPlot({
    req(input$corr_vars)
    selected_data <- spotify_data[, input$corr_vars, drop = FALSE]
    corr_matrix <- cor(selected_data, use = "complete.obs")
    
    corrplot(corr_matrix, method = "color", type = "lower",
             tl.col = "black", tl.srt = 45, number.cex = 0.7,
             addCoef.col = "black", diag = FALSE)
  })
  
  output$split_plot <- renderPlot({
    splits <- data_split()
    df_train <- splits$train
    df_valid <- splits$valid
    df_test  <- splits$test
    
    df_train$Set <- "Train"
    df_valid$Set <- "Validation"
    df_test$Set  <- "Test"
    
    combined <- bind_rows(df_train, df_valid, df_test)
    
    ggplot(combined, aes(x = Popularity, fill = Set)) +
      geom_density(alpha = 0.4) +
      theme_minimal() +
      labs(title = "Popularity Distribution by Data Split")
  })
  
  output$cv_mse_plot <- renderPlot({
    req(input$corr_vars)
    
    splits <- data_split()
    df_train <- splits$train
    df_valid <- splits$valid
    
    predictors <- setdiff(input$corr_vars, "Popularity")
    validate(need(length(predictors) >= 1, "Please select at least one predictor."))
    
    lm_mse <- function(formula, train_data, valid_data) {
      y_true <- valid_data$Popularity
      model <- lm(formula, data = train_data)
      preds <- predict(model, newdata = valid_data)
      mean((y_true - preds)^2)
    }
    
    top_formulas <- list()
    top_mses <- numeric()
    
    for (p in 1:length(predictors)) {
      formulas <- generate_formulas(p, x_vars = predictors, y_var = "Popularity")
      mses <- sapply(formulas, function(f) {
        lm_mse(as.formula(f), df_train, df_valid)
      })
      top_formulas[[p]] <- formulas[which.min(mses)]
      top_mses[p] <- min(mses)
    }
    
    best_index <- which.min(top_mses)
    best_formula <- top_formulas[[best_index]]
    best_mse <- round(top_mses[best_index], 2)
    
    output$best_model_info <- renderPrint({
      cat("Best model uses", best_index, "predictors\n")
      cat("Validation MSE:", best_mse, "\n")
      cat("Formula:\n")
      cat(best_formula, "\n")
    })
    
    df <- data.frame(NumPredictors = 1:length(predictors), MSE = top_mses)
    ggplot(df, aes(x = NumPredictors, y = MSE)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 2, color = "darkred") +
      geom_vline(xintercept = best_index, linetype = "dashed", color = "darkgreen") +
      scale_x_continuous(breaks = 1:length(predictors)) +
      labs(title = "Validation MSE vs. Number of Predictors",
           x = "Number of Predictors",
           y = "Validation MSE") +
      theme_minimal()
  })
  
  output$pred_vs_actual_plot <- renderPlot({
    req(input$corr_vars)
    
    splits <- data_split()
    df_train <- splits$train
    df_valid <- splits$valid
    df_test  <- splits$test
    
    train_valid <- bind_rows(df_train, df_valid)
    predictors <- setdiff(input$corr_vars, "Popularity")
    validate(need(length(predictors) >= 1, "Please select at least one predictor."))
    
    lm_mse <- function(formula, train_data, valid_data) {
      y_true <- valid_data$Popularity
      model <- lm(formula, data = train_data)
      preds <- predict(model, newdata = valid_data)
      mean((y_true - preds)^2)
    }
    
    top_formulas <- list()
    top_mses <- numeric()
    
    for (p in 1:length(predictors)) {
      formulas <- generate_formulas(p, x_vars = predictors, y_var = "Popularity")
      mses <- sapply(formulas, function(f) {
        lm_mse(as.formula(f), df_train, df_valid)
      })
      top_formulas[[p]] <- formulas[which.min(mses)]
      top_mses[p] <- min(mses)
    }
    
    best_index <- which.min(top_mses)
    best_formula <- top_formulas[[best_index]]
    final_model <- lm(as.formula(best_formula), data = train_valid)
    preds <- predict(final_model, newdata = df_test)
    
    results_df <- data.frame(
      Actual = df_test$Popularity,
      Predicted = preds
    )
    
    ggplot(results_df, aes(x = Predicted, y = Actual)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
      labs(title = "Predicted vs Actual Popularity (Test Set)",
           x = "Predicted Popularity",
           y = "Actual Popularity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
})