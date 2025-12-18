# granger_causality.R
# Granger causality: Do past mentions improve prediction of current volatility (magnitude)?

merged_data <- readRDS("data/combined_data.rds")
data <- merged_data[!is.na(merged_data$returns), ]

big_tech <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG")
retail_meme <- c("PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN")
semiconductor <- c("NVDA", "AMD", "AVGO", "INTC", "MU","TSM", "QCOM", "ARM")

data$category <- NA
data$category[data$ticker %in% big_tech] <- "Big Tech"
data$category[data$ticker %in% retail_meme] <- "Retail/Meme"
data$category[data$ticker %in% semiconductor] <- "Semiconductor"

# Create volatility variable (absolute returns)
data$volatility <- abs(data$returns) * 100  # As percentage

#' Granger causality test using F-test
#' @param data Data frame with date, ticker, mentions, volatility
#' @param x_var Predictor variable
#' @param y_var Response variable
#' @param max_lag Maximum number of lags to test (default 3)
#' @return List with F-statistics and p-values
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

categories <- c("Big Tech", "Retail/Meme", "Semiconductor")

results_mentions_to_volatility <- data.frame(
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

results_volatility_to_mentions <- data.frame(
  Category = character(),
  N = integer(),
  F_Stat = numeric(),
  P_Value = numeric(),
  R2_Without_Volatility = numeric(),
  R2_With_Volatility = numeric(),
  Improvement = numeric(),
  Granger_Causes = character(),
  stringsAsFactors = FALSE
)

cat("(Do past mentions help predict current volatility?)\n")
for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) < 30) {
    cat(cat_name, ": Insufficient data (n=", nrow(subset_data), ")\n\n")
    next
  }
  
  # Test: mentions help predict volatility
  result <- granger_test(subset_data, x_var = "mentions", y_var = "volatility", max_lag = 3)
  
  if (!is.na(result$f_stat)) {
    improvement <- (result$unrestricted_r2 - result$restricted_r2) / result$restricted_r2 * 100
    causes <- ifelse(result$p_value < 0.05, "YES*", "NO")
    
    cat(cat_name, ":\n")
    cat("  Sample size (after lagging):", result$n, "\n")
    cat("  F-statistic:", round(result$f_stat, 3), "\n")
    cat("  P-value:", round(result$p_value, 4), 
        ifelse(result$p_value < 0.05, " *SIGNIFICANT*", ""), "\n")
    cat("  R^2 without mentions:", round(result$restricted_r2, 4), "\n")
    cat("  R^2 with mentions:", round(result$unrestricted_r2, 4), "\n")
    cat("  Improvement:", round(improvement, 2), "%\n")
    cat("  Mentions Granger-cause volatility?", causes, "\n\n")
    
    results_mentions_to_volatility <- rbind(results_mentions_to_volatility, data.frame(
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
cat("(Do past volatility levels help predict current mentions?)\n")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) < 30) {
    cat(cat_name, ": Insufficient data (n=", nrow(subset_data), ")\n\n")
    next
  }
  
  # Test: volatility helps predict mentions
  result <- granger_test(subset_data, x_var = "volatility", y_var = "mentions", max_lag = 3)
  
  if (!is.na(result$f_stat)) {
    improvement <- (result$unrestricted_r2 - result$restricted_r2) / result$restricted_r2 * 100
    causes <- ifelse(result$p_value < 0.05, "YES*", "NO")
    
    cat(cat_name, ":\n")
    cat("  Sample size (after lagging):", result$n, "\n")
    cat("  F-statistic:", round(result$f_stat, 3), "\n")
    cat("  P-value:", round(result$p_value, 4), 
        ifelse(result$p_value < 0.05, " *SIGNIFICANT*", ""), "\n")
    cat("  R^2 without volatility:", round(result$restricted_r2, 4), "\n")
    cat("  R^2 with volatility:", round(result$unrestricted_r2, 4), "\n")
    cat("  Improvement:", round(improvement, 2), "%\n")
    cat("  Volatility Granger-causes mentions?", causes, "\n\n")
    
    results_volatility_to_mentions <- rbind(results_volatility_to_mentions, data.frame(
      Category = cat_name,
      N = result$n,
      F_Stat = result$f_stat,
      P_Value = result$p_value,
      R2_Without_Volatility = result$restricted_r2,
      R2_With_Volatility = result$unrestricted_r2,
      Improvement = improvement,
      Granger_Causes = causes
    ))
  }
}

cat("\n")
cat("SUMMARY OF CAUSALITY ANALYSIS (VOLATILITY)\n")

cat("MENTIONS help predict VOLATILITY:\n")
print(results_mentions_to_volatility)
cat("\n")

cat("VOLATILITY help predict MENTIONS:\n")
print(results_volatility_to_mentions)
cat("\n")

# Save results
write.csv(results_mentions_to_volatility, "plots/granger_mentions_to_volatility.csv", row.names = FALSE)
write.csv(results_volatility_to_mentions, "plots/granger_volatility_to_mentions.csv", row.names = FALSE)

cat("Results saved to:\n")
cat("  - plots/granger_mentions_to_volatility.csv\n")
cat("  - plots/granger_volatility_to_mentions.csv\n\n")

cat("INTERPRETATION\n")
cat("- If 'Mentions result in Volatility' is significant: Reddit discussion predicts larger price swings\n")
cat("- If 'Volatility results in Mentions' is significant: Large price moves drive Reddit discussion\n")
cat("- If BOTH significant: Bidirectional feedback loop exists\n")
cat("- If NEITHER significant: No causal relationship detected\n\n")

# Visualization
if (!dir.exists("plots")) dir.create("plots")

png("plots/granger_causality_volatility_results.png", width = 1000, height = 600)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# Plot 1: Mentions to Volatility
if (nrow(results_mentions_to_volatility) > 0) {
  barplot(results_mentions_to_volatility$F_Stat,
          names.arg = results_mentions_to_volatility$Category,
          col = ifelse(results_mentions_to_volatility$P_Value < 0.05, "darkgreen", "gray"),
          main = "Mentions - Volatility\n(Do mentions predict price swing size?)",
          ylab = "F-Statistic",
          ylim = c(0, max(results_mentions_to_volatility$F_Stat, na.rm = TRUE) * 1.2))
  
  # Add significance threshold line
  f_crit <- qf(0.95, 3, 20)  # Approximate critical value
  abline(h = f_crit, col = "red", lty = 2, lwd = 2)
  text(0.5, f_crit * 1.1, "p=0.05 threshold", col = "red", pos = 4)
}

# Plot 2: Volatility to Mentions
if (nrow(results_volatility_to_mentions) > 0) {
  barplot(results_volatility_to_mentions$F_Stat,
          names.arg = results_volatility_to_mentions$Category,
          col = ifelse(results_volatility_to_mentions$P_Value < 0.05, "darkblue", "gray"),
          main = "Volatility - Mentions\n(Do price swings predict discussion?)",
          ylab = "F-Statistic",
          ylim = c(0, max(results_volatility_to_mentions$F_Stat, na.rm = TRUE) * 1.2))
  
  abline(h = f_crit, col = "red", lty = 2, lwd = 2)
  text(0.5, f_crit * 1.1, "p=0.05 threshold", col = "red", pos = 4)
}

dev.off()
cat("Plot saved: plots/granger_causality_volatility_results.png\n\n")

# Detailed interpretation for each category
cat("DETAILED CAUSALITY PATTERNS\n")

for (i in 1:nrow(results_mentions_to_volatility)) {
  cat_name <- results_mentions_to_volatility$Category[i]
  
  mentions_causes_volatility <- results_mentions_to_volatility$Granger_Causes[i] == "YES*"
  volatility_causes_mentions <- FALSE
  
  if (cat_name %in% results_volatility_to_mentions$Category) {
    volatility_causes_mentions <- results_volatility_to_mentions$Granger_Causes[
      results_volatility_to_mentions$Category == cat_name] == "YES*"
  }
  
  cat(cat_name, ":\n")
  
  if (mentions_causes_volatility && volatility_causes_mentions) {
    cat("  BIDIRECTIONAL: Feedback loop detected\n")
    cat("  - Reddit discussion and volatility reinforce each other\n")
    cat("  - High mentions lead to bigger swings, which generate more discussion\n")
  } else if (mentions_causes_volatility) {
    cat("  PREDICTIVE: Mentions predict volatility\n")
    cat("  - High Reddit discussion precedes larger price movements\n")
    cat("  - Social media attention increases market volatility\n")
  } else if (volatility_causes_mentions) {
    cat("  REACTIVE: Volatility predicts mentions\n")
    cat("  - Large price swings drive Reddit discussion\n")
    cat("  - Traders discuss stocks after dramatic moves\n")
  } else {
    cat("  NO CAUSAL RELATIONSHIP: Correlation exists but not predictive\n")
    cat("  - Same-day correlation does not imply temporal causation\n")
    cat("  - Both may respond to common external factors\n")
  }
  cat("\n")
}
