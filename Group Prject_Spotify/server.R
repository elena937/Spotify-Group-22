library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(caret)
library(readr)
library(tidyverse)
library(glmnet)

source("generate_formulas.R")
source("categorizing_genres.R")

spotify_data <- read.csv("Spotify-2000.csv") %>%
  rename(
    BPM = `Beats.Per.Minute..BPM.`,
    Loudness = `Loudness..dB.`,
    Genre = `Top.Genre`,
    Length = `Length..Duration.`
  ) %>%
  mutate(
    Genre_Category = categorize_genres(Genre),
    across(
      c(BPM, Energy, Danceability, Loudness, Valence, Length, 
        Acousticness, Speechiness, Liveness, Popularity, Year), as.numeric
    )
  ) %>%
  select(-Index) %>%
  na.omit()

predictor_vars <- setdiff(names(spotify_data)[sapply(spotify_data, is.numeric)], "Popularity")

shinyServer(function(input, output, session) {
  
  #------------------ Initial Visualization ------------------#
  output$line_plot <- renderPlot({
    filtered_data <- spotify_data %>%
      filter(Year >= input$line_year_range[1], Year <= input$line_year_range[2])
    
    avg_popularity <- filtered_data %>%
      group_by(Year) %>%
      summarise(avg_popularity = mean(Popularity, na.rm = TRUE))
    
    ggplot(avg_popularity, aes(x = Year, y = avg_popularity)) +
      geom_line(color = "steelblue") +
      geom_smooth(method = "loess", se = FALSE, color = "darkred") +
      labs(title = "Average Popularity of Tracks Over the Years",
           x = "Year",
           y = "Average Popularity") +
      theme_minimal()
  })
  
  output$hist_plot <- renderPlot({
    ggplot(spotify_data, aes_string(x = input$hist_var)) +
      geom_histogram(binwidth = 5, fill = "darkgreen", color = "white", alpha = 0.8) +
      labs(title = paste("Distribution of", input$hist_var)) +
      theme_minimal()
  })
  
  #------------------ Correlation Heatmap ------------------#
  output$corr_plot <- renderPlot({
    req(input$corr_vars, input$heatmap_genres)
    
    selected_data <- spotify_data %>%
      filter(Genre_Category %in% input$heatmap_genres) %>%
      select(all_of(input$corr_vars))
    
    selected_data <- selected_data[, sapply(selected_data, function(x) length(unique(x)) > 1)]
    
    validate(
      need(ncol(selected_data) >= 2, "Please select at least two variables with sufficient variability.")
    )
    
    corr_matrix <- round(cor(selected_data, use = "complete.obs"), 2)
    corr_df <- as.data.frame(as.table(corr_matrix))
    names(corr_df) <- c("Var1", "Var2", "Correlation")
    
    palette_colors <- switch(input$heatmap_palette,
                             "Green/Purple" = c("green", "white", "purple"),
                             "Blue/Red" = c("blue", "white", "red"),
                             "Pink/Orange" = c("deeppink", "white", "darkorange"))
    
    ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Correlation), size = 5) +
      scale_fill_gradient2(low = palette_colors[1],
                           mid = palette_colors[2],
                           high = palette_colors[3],
                           midpoint = 0, limit = c(-1, 1), space = "Lab") +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      coord_fixed() +
      labs(title = "Correlation Heatmap", x = "", y = "")
  }, height = 600)
  
  #------------------ Data Split ------------------#
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
  
  #------------------ Delegate Other Tabs ------------------#
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
  
  best_subset_model <- reactive({
    req(input$model_vars)
    predictors <- input$model_vars
    validate(need(length(predictors) >= 1, "Please select at least one predictor."))
    
    splits <- data_split()
    df_train <- splits$train
    df_valid <- splits$valid
    df_train_valid <- bind_rows(df_train, df_valid)
    
    top_formulas <- list()
    top_mses <- numeric()
    
    for (p in 1:length(predictors)) {
      formulas <- generate_formulas(p, x_vars = predictors, y_var = "Popularity")
      mses <- sapply(formulas, function(f) {
        model <- lm(as.formula(f), data = df_train)
        preds <- predict(model, newdata = df_valid)
        mean((df_valid$Popularity - preds)^2)
      })
      top_formulas[[p]] <- formulas[which.min(mses)]
      top_mses[p] <- min(mses)
    }
    
    best_index <- which.min(top_mses)
    best_formula <- top_formulas[[best_index]]
    best_model <- lm(as.formula(best_formula), data = df_train_valid)
    best_mse <- round(top_mses[best_index], 2)
    
    list(formula = best_formula, model = best_model, best_index = best_index, mse = best_mse)
  })
  
  output$cv_mse_plot <- renderPlot({
    model_info <- best_subset_model()
    predictors <- input$model_vars
    
    df <- data.frame(NumPredictors = 1:length(predictors), MSE = rep(NA, length(predictors)))
    
    splits <- data_split()
    df_train <- splits$train
    df_valid <- splits$valid
    
    for (p in 1:length(predictors)) {
      formulas <- generate_formulas(p, x_vars = predictors, y_var = "Popularity")
      df$MSE[p] <- min(sapply(formulas, function(f) {
        model <- lm(as.formula(f), data = df_train)
        mean((df_valid$Popularity - predict(model, newdata = df_valid))^2)
      }))
    }
    
    ggplot(df, aes(x = NumPredictors, y = MSE)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 2, color = "darkred") +
      geom_vline(xintercept = model_info$best_index, linetype = "dashed", color = "darkgreen") +
      scale_x_continuous(breaks = 1:length(predictors)) +
      labs(title = "Validation MSE vs. Number of Predictors",
           x = "Number of Predictors",
           y = "Validation MSE") +
      theme_minimal()
  })
  
  output$best_model_info <- renderPrint({
    model_info <- best_subset_model()
    cat("Best model uses", model_info$best_index, "predictors\n")
    cat("Validation MSE:", model_info$mse, "\n")
    cat("Formula:\n", model_info$formula, "\n")
  })
  
  output$summary_text_ols <- renderText({
    predictors <- input$model_vars
    model_info <- best_subset_model()
    paste0("Using Best Subset Selection, we considered ", length(predictors),
           " predictor(s). The best-fit model includes ",
           model_info$best_index, " predictor(s). Summary below:")
  })
  
  output$ols_summary <- renderTable({
    model_info <- best_subset_model()
    coefs <- summary(model_info$model)$coefficients
    df <- as.data.frame(coefs)
    df <- cbind(Variable = rownames(df), df)
    rownames(df) <- NULL
    df[sapply(df, is.numeric)] <- round(df[sapply(df, is.numeric)], 3)
    df
  })
  
  output$pred_vs_actual_plot <- renderPlot({
    model_info <- best_subset_model()
    df_test <- data_split()$test
    preds <- predict(model_info$model, newdata = df_test)
    ggplot(data.frame(Actual = df_test$Popularity, Predicted = preds),
           aes(x = Predicted, y = Actual)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", size = 1) +
      labs(title = "Predicted vs Actual Popularity (Test Set)",
           x = "Predicted Popularity",
           y = "Actual Popularity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  lasso_model <- reactive({
    req(input$model_vars)
    predictors <- input$model_vars
    validate(need(length(predictors) >= 1, "Please select at least one predictor."))
    
    splits <- data_split()
    train_valid <- bind_rows(splits$train, splits$valid)
    
    x <- model.matrix(Popularity ~ ., train_valid[, c("Popularity", predictors)])[,-1]
    y <- train_valid$Popularity
    
    cvfit <- cv.glmnet(x, y, alpha = 1)
    
    list(cvfit = cvfit, predictors = predictors, train_valid = train_valid)
  })
  
  output$cv_lasso_plot <- renderPlot({
    model_info <- lasso_model()
    plot(model_info$cvfit)
    title("Lasso CV Error")
  })
  
  output$lasso_model_info <- renderPrint({
    model_info <- lasso_model()
    cat("Best lambda:", model_info$cvfit$lambda.min, "\n")
    cat("CV MSE:", min(model_info$cvfit$cvm), "\n")
  })
  
  output$lasso_summary_text <- renderText({
    model_info <- lasso_model()
    coefs <- coef(model_info$cvfit, s = model_info$cvfit$lambda.min)
    selected_vars <- rownames(coefs)[coefs[, 1] != 0]
    selected_vars <- selected_vars[!selected_vars %in% "(Intercept)"]
    paste0("Using Lasso, we considered ", length(model_info$predictors),
           " predictor(s). The final model includes ", length(selected_vars),
           " predictor(s). Summary below:")
  })
  
  output$lasso_summary <- renderTable({
    model_info <- lasso_model()
    coefs <- coef(model_info$cvfit, s = model_info$cvfit$lambda.min)
    selected_vars <- rownames(coefs)[coefs[, 1] != 0]
    selected_vars <- selected_vars[!selected_vars %in% "(Intercept)"]
    
    if (length(selected_vars) == 0) {
      return(data.frame(Notice = "No variables selected by Lasso"))
    }
    
    formula <- as.formula(paste("Popularity ~", paste(selected_vars, collapse = " + ")))
    model <- lm(formula, data = model_info$train_valid)
    
    df <- summary(model)$coefficients
    df <- cbind(Variable = rownames(df), as.data.frame(df))
    rownames(df) <- NULL
    df[sapply(df, is.numeric)] <- round(df[sapply(df, is.numeric)], 3)
    df
  })
  
  output$lasso_pred_vs_actual <- renderPlot({
    model_info <- lasso_model()
    splits <- data_split()
    df_test <- splits$test
    predictors <- model_info$predictors
    
    x_test <- model.matrix(Popularity ~ ., df_test[, c("Popularity", predictors)])[,-1]
    preds <- predict(model_info$cvfit, s = model_info$cvfit$lambda.min, newx = x_test)
    
    ggplot(data.frame(Actual = df_test$Popularity, Predicted = as.numeric(preds)),
           aes(x = Predicted, y = Actual)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "blue", size = 1) +
      labs(title = "Predicted vs Actual Popularity (Lasso Test Set)",
           x = "Predicted Popularity",
           y = "Actual Popularity") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
})
