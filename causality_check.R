# granger_causality.R
# Granger causality: Do past mentions improve prediction of current returns?

merged_data <- readRDS("data/combined_data.rds")
data <- merged_data[!is.na(merged_data$returns), ]

mega_cap <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG")
retail_meme <- c("PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN")
semiconductor <- c("NVDA", "AMD", "AVGO", "INTC", "MU","TSM", "QCOM", "ARM")

data$category <- NA
data$category[data$ticker %in% mega_cap] <- "Mega-cap"
data$category[data$ticker %in% retail_meme] <- "Retail/Meme"
data$category[data$ticker %in% semiconductor] <- "Semiconductor"

cat("GRANGER CAUSALITY TEST\n\n")
# Question 1: Do past Reddit mentions predict current stock returns?
# Question 2: Do past stock returns predict current Reddit mentions?

#' Granger causality test using F-test
#' @param data Data frame with date, ticker, mentions, returns
#' @param max_lag Maximum number of lags to test (default 3)
#' @return Data frame with F-statistics and p-values
granger_test <- function(data, x_var, y_var, max_lag = 3) {
  # Sort by ticker and date
  data <- data[order(data$ticker, data$date), ]
  
  # Create lagged variables
  for (lag in 1:max_lag) {
    data[[paste0(x_var, "_lag", lag)]] <- NA
    data[[paste0(y_var, "_lag", lag)]] <- NA
    
    for (ticker in unique(data$ticker)) {
      idx <- which(data$ticker == ticker)
      if (length(idx) > lag) {
        data[[paste0(x_var, "_lag", lag)]][idx[(lag+1):length(idx)]] <- 
          data[[x_var]][idx[1:(length(idx)-lag)]]
        data[[paste0(y_var, "_lag", lag)]][idx[(lag+1):length(idx)]] <- 
          data[[y_var]][idx[1:(length(idx)-lag)]]
      }
    }
  }
  
  # Remove rows with NA lags
  complete_data <- data[complete.cases(data[, grep("_lag", names(data))]), ]
  
  if (nrow(complete_data) < 20) {
    return(list(f_stat = NA, p_value = NA, n = nrow(complete_data)))
  }
  
  # Restricted model: y ~ lagged_y only
  restricted_formula <- paste0(y_var, " ~ ", 
                               paste0(y_var, "_lag", 1:max_lag, collapse = " + "))
  model_restricted <- lm(as.formula(restricted_formula), data = complete_data)
  
  # Unrestricted model: y ~ lagged_y + lagged_x
  unrestricted_formula <- paste0(restricted_formula, " + ",
                                 paste0(x_var, "_lag", 1:max_lag, collapse = " + "))
  model_unrestricted <- lm(as.formula(unrestricted_formula), data = complete_data)
  
  # F-test: Does adding lagged_x improve prediction?
  f_test <- anova(model_restricted, model_unrestricted)
  f_stat <- f_test$F[2]
  p_value <- f_test$`Pr(>F)`[2]
  
  return(list(
    f_stat = f_stat,
    p_value = p_value,
    n = nrow(complete_data),
    restricted_r2 = summary(model_restricted)$r.squared,
    unrestricted_r2 = summary(model_unrestricted)$r.squared
  ))
}

categories <- c("Mega-cap", "Retail/Meme", "Semiconductor")

results_mentions_to_returns <- data.frame(
  Category = character(),
  N = integer(),
  F_Stat = numeric(),
  P_Value = numeric(),
  R2_Without_Mentions = numeric(),
  R2_With_Mentions = numeric(),
  Improvement = numeric(),
  Granger_Causes = character(),
  stringsAsFactors = FALSE
)

results_returns_to_mentions <- data.frame(
  Category = character(),
  N = integer(),
  F_Stat = numeric(),
  P_Value = numeric(),
  R2_Without_Returns = numeric(),
  R2_With_Returns = numeric(),
  Improvement = numeric(),
  Granger_Causes = character(),
  stringsAsFactors = FALSE
)

cat("DIRECTION 1: Mentions result in Returns\n")
cat("(Do past mentions help predict current returns?)\n\n")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) < 30) {
    cat(cat_name, ": Insufficient data (n=", nrow(subset_data), ")\n\n")
    next
  }
  
  # Test: mentions result in returns
  result <- granger_test(subset_data, x_var = "mentions", y_var = "returns", max_lag = 3)
  
  if (!is.na(result$f_stat)) {
    improvement <- (result$unrestricted_r2 - result$restricted_r2) / result$restricted_r2 * 100
    causes <- ifelse(result$p_value < 0.05, "YES*", "NO")
    
    cat(cat_name, ":\n")
    cat("  Sample size (after lagging):", result$n, "\n")
    cat("  F-statistic:", round(result$f_stat, 3), "\n")
    cat("  P-value:", round(result$p_value, 4), 
        ifelse(result$p_value < 0.05, " *SIGNIFICANT*", ""), "\n")
    cat("  R² without mentions:", round(result$restricted_r2, 4), "\n")
    cat("  R² with mentions:", round(result$unrestricted_r2, 4), "\n")
    cat("  Improvement:", round(improvement, 2), "%\n")
    cat("  result in Mentions Granger-cause returns?", causes, "\n\n")
    
    results_mentions_to_returns <- rbind(results_mentions_to_returns, data.frame(
      Category = cat_name,
      N = result$n,
      F_Stat = result$f_stat,
      P_Value = result$p_value,
      R2_Without_Mentions = result$restricted_r2,
      R2_With_Mentions = result$unrestricted_r2,
      Improvement = improvement,
      Granger_Causes = causes
    ))
  }
}

cat("\n")
cat("DIRECTION 2: Returns result in Mentions\n")
cat("(Do past returns help predict current mentions?)\n\n")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) < 30) {
    cat(cat_name, ": Insufficient data (n=", nrow(subset_data), ")\n\n")
    next
  }
  
  # Test: returns result in mentions
  result <- granger_test(subset_data, x_var = "returns", y_var = "mentions", max_lag = 3)
  
  if (!is.na(result$f_stat)) {
    improvement <- (result$unrestricted_r2 - result$restricted_r2) / result$restricted_r2 * 100
    causes <- ifelse(result$p_value < 0.05, "YES*", "NO")
    
    cat(cat_name, ":\n")
    cat("  Sample size (after lagging):", result$n, "\n")
    cat("  F-statistic:", round(result$f_stat, 3), "\n")
    cat("  P-value:", round(result$p_value, 4), 
        ifelse(result$p_value < 0.05, " *SIGNIFICANT*", ""), "\n")
    cat("  R² without returns:", round(result$restricted_r2, 4), "\n")
    cat("  R² with returns:", round(result$unrestricted_r2, 4), "\n")
    cat("  Improvement:", round(improvement, 2), "%\n")
    cat("  result in Returns Granger-cause mentions?", causes, "\n\n")
    
    results_returns_to_mentions <- rbind(results_returns_to_mentions, data.frame(
      Category = cat_name,
      N = result$n,
      F_Stat = result$f_stat,
      P_Value = result$p_value,
      R2_Without_Returns = result$restricted_r2,
      R2_With_Returns = result$unrestricted_r2,
      Improvement = improvement,
      Granger_Causes = causes
    ))
  }
}

cat("\nSummary of Causality Analysis:\n\n")

cat("MENTIONS result in RETURNS:\n")
print(results_mentions_to_returns)
cat("\n")

cat("RETURNS result in MENTIONS:\n")
print(results_returns_to_mentions)
cat("\n")

# Save results
write.csv(results_mentions_to_returns, "plots/granger_mentions_to_returns.csv", row.names = FALSE)
write.csv(results_returns_to_mentions, "plots/granger_returns_to_mentions.csv", row.names = FALSE)

cat("\nINTERPRETATION:\n")
cat("- If 'Mentions result in Returns' is significant: Reddit discussion predicts future price moves\n")
cat("- If 'Returns result in Mentions' is significant: Price moves drive Reddit discussion\n")
cat("- If BOTH significant: Bidirectional feedback loop exists\n")
cat("- If NEITHER significant: No causal relationship detected\n\n")

# Visualization
if (!dir.exists("plots")) dir.create("plots")

png("plots/granger_causality_results.png", width = 1000, height = 600)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# Plot 1: Mentions result in Returns
if (nrow(results_mentions_to_returns) > 0) {
  barplot(results_mentions_to_returns$F_Stat,
          names.arg = results_mentions_to_returns$Category,
          col = ifelse(results_mentions_to_returns$P_Value < 0.05, "darkgreen", "gray"),
          main = "Mentions result in Returns\n(Do mentions predict returns?)",
          ylab = "F-Statistic",
          ylim = c(0, max(results_mentions_to_returns$F_Stat) * 1.2))
  
  # Add significance threshold line
  f_crit <- qf(0.95, 3, 20)  # Approximate critical value
  abline(h = f_crit, col = "red", lty = 2, lwd = 2)
  text(0.5, f_crit * 1.1, "p=0.05 threshold", col = "red", pos = 4)
}

# Plot 2: Returns result in Mentions
if (nrow(results_returns_to_mentions) > 0) {
  barplot(results_returns_to_mentions$F_Stat,
          names.arg = results_returns_to_mentions$Category,
          col = ifelse(results_returns_to_mentions$P_Value < 0.05, "darkblue", "gray"),
          main = "Returns result in Mentions\n(Do returns predict mentions?)",
          ylab = "F-Statistic",
          ylim = c(0, max(results_returns_to_mentions$F_Stat) * 1.2))
  
  abline(h = f_crit, col = "red", lty = 2, lwd = 2)
  text(0.5, f_crit * 1.1, "p=0.05 threshold", col = "red", pos = 4)
}

dev.off()
cat("Saved: plots/granger_causality_results.png\n")


for (i in 1:nrow(results_mentions_to_returns)) {
  cat_name <- results_mentions_to_returns$Category[i]
  
  mentions_causes <- results_mentions_to_returns$Granger_Causes[i] == "YES*"
  returns_causes <- FALSE
  
  if (cat_name %in% results_returns_to_mentions$Category) {
    returns_causes <- results_returns_to_mentions$Granger_Causes[
      results_returns_to_mentions$Category == cat_name] == "YES*"
  }
  
  cat(cat_name, ":\n")
  
  if (mentions_causes && returns_causes) {
    cat("  ✓ BIDIRECTIONAL: Feedback loop detected\n")
    cat("    Reddit discussion and price movements reinforce each other\n")
  } else if (mentions_causes) {
    cat("  ✓ PREDICTIVE: Mentions predict returns\n")
    cat("    Reddit discussion has leading indicator value\n")
  } else if (returns_causes) {
    cat("  ✓ REACTIVE: Returns predict mentions\n")
    cat("    Price movements drive Reddit discussion\n")
  } else {
    cat("  ✗ No causal relationship detected\n")
    cat("    Correlation exists but not predictive\n")
  }
  cat("\n")
}