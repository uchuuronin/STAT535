# lagged_exploration.R
# Explore lagged relationships: Do past mentions predict future returns?
# Combines correlation analysis + prediction model with proper time lags

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

cat("LAGGED ANALYSIS: Do past mentions predict future volatility?\n\n")

# PART 1: LAG CORRELATION ANALYSIS
cat("Exploring Different Lag Structures\n")

# Sort data
data <- data[order(data$ticker, data$date), ]

# Create lagged mention variables (1-5 days)
for (lag in 1:5) {
  data[[paste0("mentions_lag", lag)]] <- NA
  
  for (ticker in unique(data$ticker)) {
    idx <- which(data$ticker == ticker)
    if (length(idx) > lag) {
      data[[paste0("mentions_lag", lag)]][idx[(lag+1):length(idx)]] <- 
        data$mentions[idx[1:(length(idx)-lag)]]
    }
  }
}

categories <- c("Mega-cap", "Retail/Meme", "Semiconductor")

lag_results <- data.frame(
  Category = character(),
  Lag = integer(),
  N = integer(),
  Correlation = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  cat("\n", cat_name, ":\n", sep="")
  cat("  Testing lags 1-5 days...\n")
  
  for (lag in 1:5) {
    lag_col <- paste0("mentions_lag", lag)
    valid_data <- subset_data[!is.na(subset_data[[lag_col]]), ]
    
    if (nrow(valid_data) < 10) next
    
    cor_val <- cor(valid_data[[lag_col]], valid_data$abs_return)
    model <- lm(abs_return ~ get(lag_col), data = valid_data)
    p_val <- summary(model)$coefficients[2, 4]
    
    cat(sprintf("    Lag %d: r = %6.4f, p = %.4f, n = %d", 
                lag, cor_val, p_val, nrow(valid_data)))
    if (p_val < 0.05) cat(" *")
    cat("\n")
    
    lag_results <- rbind(lag_results, data.frame(
      Category = cat_name,
      Lag = lag,
      N = nrow(valid_data),
      Correlation = cor_val,
      P_Value = p_val
    ))
  }
}

cat("\n")
write.csv(lag_results, "plots/lag_correlation_results.csv", row.names = FALSE)
cat("Saved: plots/lag_correlation_results.csv\n\n")

# Identify optimal lag per category
cat("Optimal lag (strongest correlation):\n")
for (cat_name in categories) {
  cat_results <- lag_results[lag_results$Category == cat_name, ]
  if (nrow(cat_results) > 0) {
    best_lag <- cat_results[which.max(abs(cat_results$Correlation)), ]
    cat(sprintf("  %s: Lag %d (r = %.4f, p = %.4f)\n", 
                cat_name, best_lag$Lag, best_lag$Correlation, best_lag$P_Value))
  }
}

# PART 2: LAGGED PREDICTION MODEL
cat("\n")
cat("Predictive Model with Lag-1 (Yesterday's Mentions result in Today's Volatility)\n")

# Use lag-1 for prediction (most interpretable)
prediction_data <- data[!is.na(data$mentions_lag1), ]

date_span <- as.numeric(difftime(max(prediction_data$date), 
                                  min(prediction_data$date), units = "days"))

if (date_span < 150) {
  cat("WARNING: Only", date_span, "days of data.\n")
  cat("Prediction results may be unstable. Need ~180+ days.\n\n")
}

prediction_data <- prediction_data[order(prediction_data$date), ]

# Use row-based split
n_total <- nrow(prediction_data)
n_train <- round(n_total * 0.70)

train <- prediction_data[1:n_train, ]
test <- prediction_data[(n_train + 1):n_total, ]

cat("Training set:", nrow(train), "obs,", format(min(train$date)), "to", format(max(train$date)), "\n")
cat("Test set:", nrow(test), "obs,", format(min(test$date)), "to", format(max(test$date)), "\n\n")

prediction_results <- data.frame(
  Category = character(),
  Train_N = integer(),
  Test_N = integer(),
  MAE = numeric(),
  Baseline_MAE = numeric(),
  Improvement_Pct = numeric(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  stringsAsFactors = FALSE
)

for (cat_name in categories) {
  train_cat <- train[train$category == cat_name & !is.na(train$category), ]
  test_cat <- test[test$category == cat_name & !is.na(test$category), ]
  
  if (nrow(train_cat) < 20 || nrow(test_cat) < 10) {
    cat(cat_name, ": Insufficient data\n\n")
    next
  }
  
  # Model: Today's volatility ~ Yesterday's mentions
  model <- lm(abs_return ~ mentions_lag1, data = train_cat)
  
  # Predictions
  test_cat$predicted_abs_return <- predict(model, newdata = test_cat)
  
  # Metrics
  mae <- mean(abs(test_cat$abs_return - test_cat$predicted_abs_return))
  baseline_mae <- mean(abs(test_cat$abs_return - mean(train_cat$abs_return)))
  improvement <- (baseline_mae - mae) / baseline_mae * 100
  
  cat(cat_name, ":\n")
  cat("  Train: n =", nrow(train_cat), "\n")
  cat("  Test: n =", nrow(test_cat), "\n")
  cat("  MAE:", round(mae, 3), "% (Baseline:", round(baseline_mae, 3), "%)\n")
  cat("  Improvement:", round(improvement, 1), "%\n")
  
  # Binary classification: Can we predict big moves?
  big_move_threshold <- quantile(train_cat$abs_return, 0.67)
  test_cat$actual_big_move <- test_cat$abs_return > big_move_threshold
  test_cat$predicted_big_move <- test_cat$predicted_abs_return > big_move_threshold
  
  confusion <- table(Predicted = test_cat$predicted_big_move, 
                     Actual = test_cat$actual_big_move)
  
  accuracy <- precision <- recall <- NA
  
  if (nrow(confusion) == 2 && ncol(confusion) == 2) {
    accuracy <- sum(diag(confusion)) / sum(confusion)
    precision <- confusion[2,2] / sum(confusion[2,])
    recall <- confusion[2,2] / sum(confusion[,2])
    
    cat("  Big move classification (>", round(big_move_threshold, 2), "%):\n")
    cat("    Accuracy:", round(accuracy * 100, 1), "%\n")
    cat("    Precision:", round(precision * 100, 1), "%\n")
    cat("    Recall:", round(recall * 100, 1), "%\n")
  }
  
  if (improvement > 5 && !is.na(accuracy) && accuracy > 0.6) {
    cat("Mentions have predictive value\n")
  } else {
    cat("LIMITED: Weak predictive power\n")
  }
  cat("\n")
  
  prediction_results <- rbind(prediction_results, data.frame(
    Category = cat_name,
    Train_N = nrow(train_cat),
    Test_N = nrow(test_cat),
    MAE = mae,
    Baseline_MAE = baseline_mae,
    Improvement_Pct = improvement,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall
  ))
}

write.csv(prediction_results, "plots/lagged_prediction_results.csv", row.names = FALSE)
cat("Saved: plots/lagged_prediction_results.csv\n\n")

# VISUALIZATION
if (!dir.exists("plots")) dir.create("plots")

# Plot 1: Lag correlation heatmap
png("plots/lag_correlation_heatmap.png", width = 1000, height = 600)
par(mar = c(5, 5, 4, 2))

lag_matrix <- matrix(NA, nrow = 5, ncol = 3)
colnames(lag_matrix) <- categories
rownames(lag_matrix) <- paste("Lag", 1:5)

for (i in 1:nrow(lag_results)) {
  lag <- lag_results$Lag[i]
  cat <- lag_results$Category[i]
  lag_matrix[lag, cat] <- lag_results$Correlation[i]
}

image(1:3, 1:5, t(lag_matrix), 
      col = colorRampPalette(c("blue", "white", "red"))(20),
      xlab = "", ylab = "Lag (days)", main = "Correlation: Lagged Mentions vs Volatility",
      axes = FALSE)
axis(1, at = 1:3, labels = categories, las = 2)
axis(2, at = 1:5, labels = paste("Lag", 1:5))

# Add correlation values
for (i in 1:3) {
  for (j in 1:5) {
    if (!is.na(lag_matrix[j, i])) {
      text(i, j, round(lag_matrix[j, i], 3), cex = 1.2)
    }
  }
}

dev.off()
cat("Saved: plots/lag_correlation_heatmap.png\n")

# Plot 2: Actual vs Predicted (test set)
png("plots/lagged_prediction_scatter.png", width = 1400, height = 450)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

for (cat_name in categories) {
  train_cat <- train[train$category == cat_name & !is.na(train$category), ]
  test_cat <- test[test$category == cat_name & !is.na(test$category), ]
  
  if (nrow(train_cat) >= 20 && nrow(test_cat) >= 10) {
    model <- lm(abs_return ~ mentions_lag1, data = train_cat)
    test_cat$predicted <- predict(model, newdata = test_cat)
    
    plot(test_cat$predicted, test_cat$abs_return,
         pch = 16, col = rgb(0, 0, 0, 0.5),
         xlab = "Predicted Volatility (%)",
         ylab = "Actual Volatility (%)",
         main = paste(cat_name, "(Test Set)"),
         xlim = range(c(test_cat$predicted, test_cat$abs_return)),
         ylim = range(c(test_cat$predicted, test_cat$abs_return)))
    
    abline(0, 1, col = "red", lwd = 2, lty = 2)  # Perfect prediction line
    
    mae <- mean(abs(test_cat$abs_return - test_cat$predicted))
    text(min(test_cat$predicted), max(test_cat$abs_return) * 0.95,
         paste("MAE =", round(mae, 2), "%"), pos = 4, cex = 1.1)
  }
}

dev.off()
cat("Saved: plots/lagged_prediction_scatter.png\n\n")

# SUMMARY
cat("=" ,rep("=", 70), "\n", sep="")
cat("SUMMARY\n")
cat("=" ,rep("=", 70), "\n\n", sep="")

cat("KEY FINDINGS:\n\n")

cat("LAG STRUCTURE:\n")
for (cat_name in categories) {
  cat_results <- lag_results[lag_results$Category == cat_name, ]
  sig_lags <- cat_results[cat_results$P_Value < 0.05, ]
  if (nrow(sig_lags) > 0) {
    cat("   ", cat_name, ": Significant at lags", paste(sig_lags$Lag, collapse=", "), "\n")
  } else {
    cat("   ", cat_name, ": No significant lags\n")
  }
}

cat("\nPREDICTION PERFORMANCE:\n")
for (i in 1:nrow(prediction_results)) {
  cat("   ", prediction_results$Category[i], ":\n")
  cat("      Improvement:", round(prediction_results$Improvement_Pct[i], 1), "%\n")
  if (!is.na(prediction_results$Accuracy[i])) {
    cat("      Classification accuracy:", round(prediction_results$Accuracy[i] * 100, 1), "%\n")
  }
}