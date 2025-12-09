# Analyze correlation between Reddit mentions and returns by stock type

# Load data
merged_data <- readRDS("data/combined_data.rds")

# Remove NA returns
data <- merged_data[!is.na(merged_data$returns), ]

cat("Total observations:", nrow(data), "\n\n")

# Define stock categories based on our analysis plan
mega_cap <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG")
retail_meme <- c("PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN")
semiconductor <- c("NVDA", "AMD", "AVGO", "INTC", "MU","TSM", "QCOM", "ARM")
etf <- c("SPY", "QQQ", "VOO", "VTI")

# Add category column
data$category <- NA
data$category[data$ticker %in% mega_cap] <- "Mega-cap"
data$category[data$ticker %in% retail_meme] <- "Retail/Meme"
data$category[data$ticker %in% semiconductor] <- "Semiconductor"
data$category[data$ticker %in% etf] <- "ETF"

# Print breakdown
cat("Observations by category:\n")
print(table(data$category, useNA = "ifany"))

cor_overall <- cor(data$mentions, data$returns)
cat("Correlation:", round(cor_overall, 4), "\n")

# Simple regression
model_overall <- lm(returns ~ mentions, data = data)
cat("\nLinear regression:\n")
print(summary(model_overall))

cat("\nCorr by category:\n")
# Exclude ETFs from correlation analysis
analysis_data <- data[data$category != "ETF" | is.na(data$category), ]

categories <- unique(analysis_data$category[!is.na(analysis_data$category)])

for (cat_name in categories) {
  cat("\n", cat_name, " stocks:\n", sep = "")
  
  subset_data <- analysis_data[analysis_data$category == cat_name & !is.na(analysis_data$category), ]
  
  if (nrow(subset_data) < 3) {
    cat("  Not enough data (n =", nrow(subset_data), ")\n")
    next
  }
  
  cat("  n =", nrow(subset_data), "\n")
  cat("  Correlation:", round(cor(subset_data$mentions, subset_data$returns), 4), "\n")
  
  # Regression for this category
  model <- lm(returns ~ mentions, data = subset_data)
  coef_mentions <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, 4]
  
  cat("  Beta (mentions):", round(coef_mentions, 6), "\n")
  cat("  P-value:", round(p_value, 4), "\n")
  
  if (p_value < 0.05) {
    cat("  ! Statistically significant at 5% level\n")
  }
}

cat("\nHIGH MENTION DAYS (mentions >= 3):\n")
high_mention_data <- data[data$mentions >= 3, ]

if (nrow(high_mention_data) > 5) {
  cat("n =", nrow(high_mention_data), "\n")
  cor_high <- cor(high_mention_data$mentions, high_mention_data$returns)
  cat("Correlation:", round(cor_high, 4), "\n")
  
  model_high <- lm(returns ~ mentions, data = high_mention_data)
  print(summary(model_high))
} else {
  cat("Not enough high-mention observations\n")
}

cat("\nLAGGED ANALYSIS (next-day returns)\n")

# Sort by ticker and date
data <- data[order(data$ticker, data$date), ]

# Create lagged returns (shift returns back one day per ticker)
data$next_day_return <- NA

for (t in unique(data$ticker)) {
  idx <- which(data$ticker == t)
  if (length(idx) > 1) {
    data$next_day_return[idx[-length(idx)]] <- data$returns[idx[-1]]
  }
}

# Remove rows without next-day data
lagged_data <- data[!is.na(data$next_day_return), ]

if (nrow(lagged_data) > 5) {
  cat("n =", nrow(lagged_data), "\n")
  cor_lagged <- cor(lagged_data$mentions, lagged_data$next_day_return)
  cat("Correlation:", round(cor_lagged, 4), "\n")
  
  model_lagged <- lm(next_day_return ~ mentions, data = lagged_data)
  cat("\nLinear regression:\n")
  print(summary(model_lagged))
} else {
  cat("Not enough data for lagged analysis\n")
}

cat("\n\nAnalysis Type          | n  | Correlation | P-value\n")
cat("------------------------------------------------\n")

# Overall
cat(sprintf("%-22s | %2d | %11.4f | %7.4f\n", 
            "Overall", nrow(data), cor_overall, 
            summary(model_overall)$coefficients[2, 4]))

# By category
for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  if (nrow(subset_data) >= 3) {
    model <- lm(returns ~ mentions, data = subset_data)
    cat(sprintf("%-22s | %2d | %11.4f | %7.4f\n", 
                cat_name, nrow(subset_data), 
                cor(subset_data$mentions, subset_data$returns),
                summary(model)$coefficients[2, 4]))
  }
}

# High mentions
if (nrow(high_mention_data) > 5) {
  cat(sprintf("%-22s | %2d | %11.4f | %7.4f\n", 
              "High mentions (>=3)", nrow(high_mention_data), cor_high,
              summary(model_high)$coefficients[2, 4]))
}

# Lagged
if (nrow(lagged_data) > 5) {
  cat(sprintf("%-22s | %2d | %11.4f | %7.4f\n", 
              "Lagged (next-day)", nrow(lagged_data), cor_lagged,
              summary(model_lagged)$coefficients[2, 4]))
}


data_reverse <- data[order(data$ticker, data$date), ]

# Initialize column
data_reverse$next_day_mentions <- NA

# Create lagged mentions (shift mentions back one day per ticker)

for (t in unique(data_reverse$ticker)) {
  idx <- which(data_reverse$ticker == t)
  if (length(idx) > 1) {
    # Shift mentions forward (today's return corr. tomorrow's mentions)
    data_reverse$next_day_mentions[idx[-length(idx)]] <- data_reverse$mentions[idx[-1]]
  }
}
# Remove rows without next-day data
reverse_data <- data_reverse[!is.na(data_reverse$next_day_mentions) & !is.na(data_reverse$returns), ]

if (nrow(reverse_data) > 5) {
  cat("n =", nrow(reverse_data), "\n")
  cor_reverse <- cor(reverse_data$returns, reverse_data$next_day_mentions)
  cat("Correlation (returns result in next-day mentions):", round(cor_reverse, 4), "\n")
  
  model_reverse <- lm(next_day_mentions ~ returns, data = reverse_data)
  cat("\nLinear regression:\n")
  print(summary(model_reverse))
  
  # Interpretation
  if (summary(model_reverse)$coefficients[2, 4] < 0.05) {
    cat("\n! SIGNIFICANT: Stock movements do impact Reddit discussion!\n")
  } else {
    cat("\nNot significant: Stock movements don't strongly predict discussion\n")
  }
  
  # Test by category
  cat("\nBy Category -\n")
  for (cat_name in c("Retail/Meme", "Mega-cap", "Semiconductor")) {
    subset_reverse <- reverse_data[reverse_data$category == cat_name & !is.na(reverse_data$category), ]
    if (nrow(subset_reverse) >= 5) {
      cor_val <- cor(subset_reverse$returns, subset_reverse$next_day_mentions)
      cat(sprintf("%-15s (n=%2d): r = %6.4f\n", cat_name, nrow(subset_reverse), cor_val))
    }
  }
  
} else {
  cat("Not enough data for reverse lag analysis\n")
}

if (nrow(reverse_data) >= 10) {
  png("plots/returns_to_mentions.png", width = 800, height = 600)
  par(mar = c(4, 4, 3, 1))
  
  plot(reverse_data$returns, 
       reverse_data$next_day_mentions,
       pch = 16,
       col = rgb(0, 0.5, 0, 0.3),
       xlab = "Today's Return",
       ylab = "Next-Day Reddit Mentions",
       main = "Do Stock Movements Predict Reddit Discussion?")
  
  abline(v = 0, col = "gray", lty = 2)
  abline(lm(next_day_mentions ~ returns, data = reverse_data), 
         col = "darkgreen", lwd = 2)
  
  dev.off()
  cat("\nPlot saved to: plots/stock_spikes_to_mentions.png\n")
}