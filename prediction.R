# Predictive model: Can mentions forecast volatility?
# Only build if magnitude analysis shows promise

merged_data <- readRDS("data/combined_data.rds")
data <- merged_data[!is.na(merged_data$returns), ]

mega_cap <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG")
retail_meme <- c("PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN")
semiconductor <- c("NVDA", "AMD", "AVGO", "INTC", "MU","TSM", "QCOM", "ARM")

data$category <- NA
data$category[data$ticker %in% mega_cap] <- "Mega-cap"
data$category[data$ticker %in% retail_meme] <- "Retail/Meme"
data$category[data$ticker %in% semiconductor] <- "Semiconductor"

data$abs_return <- abs(data$returns) * 100

# Only proceed if we have 6+ months of data
date_span <- as.numeric(difftime(max(data$date), min(data$date), units = "days"))
if (date_span < 150) {
  cat("WARNING: Only", date_span, "days of data. Need ~180 days for reliable prediction.\n")
  cat("Run reddit_scraper.R with n_pages=20 first.\n")
  quit(save = "no")
}

cat("PREDICTIVE MODEL: MENTIONS result in VOLATILITY\n\n")

# TRAIN/TEST SPLIT (70/30 by date)
split_date <- quantile(data$date, 0.70)
train <- data[data$date <= split_date, ]
test <- data[data$date > split_date, ]

cat("Training set:", nrow(train), "obs,", format(min(train$date)), "to", format(max(train$date)), "\n")
cat("Test set:", nrow(test), "obs,", format(min(test$date)), "to", format(max(test$date)), "\n\n")

categories <- c("Mega-cap", "Retail/Meme", "Semiconductor")

for (cat_name in categories) {
  train_cat <- train[train$category == cat_name & !is.na(train$category), ]
  test_cat <- test[test$category == cat_name & !is.na(test$category), ]
  
  if (nrow(train_cat) < 20 || nrow(test_cat) < 10) {
    cat(cat_name, ": Insufficient data for prediction\n\n")
    next
  }
  
  # Simple linear model: abs_return ~ mentions
  model <- lm(abs_return ~ mentions, data = train_cat)
  
  # Predictions on test set
  test_cat$predicted_abs_return <- predict(model, newdata = test_cat)
  
  # Evaluation metrics
  mae <- mean(abs(test_cat$abs_return - test_cat$predicted_abs_return))
  rmse <- sqrt(mean((test_cat$abs_return - test_cat$predicted_abs_return)^2))
  baseline_mae <- mean(abs(test_cat$abs_return - mean(train_cat$abs_return)))
  
  improvement <- (baseline_mae - mae) / baseline_mae * 100
  
  cat(cat_name, ":\n")
  cat("  MAE:", round(mae, 3), "% (baseline:", round(baseline_mae, 3), "%)\n")
  cat("  RMSE:", round(rmse, 3), "%\n")
  cat("  Improvement over baseline:", round(improvement, 1), "%\n")
  
  if (improvement > 5) {
    cat("Mentions have PRACTICAL predictive value\n")
  } else {
    cat("Mentions have LIMITED predictive value\n")
  }
  cat("\n")
  
  # THRESHOLD-BASED CLASSIFICATION: Can we predict big moves?
  # Define "big move" as top 33% of training set
  big_move_threshold <- quantile(train_cat$abs_return, 0.67)
  
  test_cat$actual_big_move <- test_cat$abs_return > big_move_threshold
  test_cat$predicted_big_move <- test_cat$predicted_abs_return > big_move_threshold
  
  confusion <- table(Predicted = test_cat$predicted_big_move, 
                     Actual = test_cat$actual_big_move)
  
  if (nrow(confusion) == 2 && ncol(confusion) == 2) {
    accuracy <- sum(diag(confusion)) / sum(confusion)
    precision <- confusion[2,2] / sum(confusion[2,])
    recall <- confusion[2,2] / sum(confusion[,2])
    
    cat("  Big move classification (threshold:", round(big_move_threshold, 2), "%):\n")
    cat("    Accuracy:", round(accuracy * 100, 1), "%\n")
    cat("    Precision:", round(precision * 100, 1), "%\n")
    cat("    Recall:", round(recall * 100, 1), "%\n\n")
  }
}

cat("\nCONCLUSION:\n")
cat("If improvement >5% and accuracy >60%, mentions have predictive value for volatility.\n")
cat("This suggests Reddit discussion intensity signals upcoming price turbulence.\n")