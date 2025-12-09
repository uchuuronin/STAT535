# Analyze magnitude of stock movements (not direction) vs Reddit mentions

merged_data <- readRDS("data/combined_data.rds")
data <- merged_data[!is.na(merged_data$returns), ]

# Define categories
mega_cap <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG")
retail_meme <- c("PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN")
semiconductor <- c("NVDA", "AMD", "AVGO", "INTC", "MU","TSM", "QCOM", "ARM")

data$category <- NA
data$category[data$ticker %in% mega_cap] <- "Mega-cap"
data$category[data$ticker %in% retail_meme] <- "Retail/Meme"
data$category[data$ticker %in% semiconductor] <- "Semiconductor"

# MAGNITUDE: Use absolute returns
data$abs_return <- abs(data$returns) * 100  # Convert to percentage

cat("MAGNITUDE ANALYSIS\n\n")
cat("Question: Do higher mentions predict larger price movements?\n\n")

categories <- c("Mega-cap", "Retail/Meme", "Semiconductor")

results <- data.frame(
  Category = character(),
  N = integer(),
  Corr_Abs_Return = numeric(),
  P_Value = numeric(),
  Avg_Move_Low_Mentions = numeric(),
  Avg_Move_High_Mentions = numeric(),
  Ratio = numeric(),
  stringsAsFactors = FALSE
)

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) < 10) {
    cat(cat_name, ": Insufficient data (n=", nrow(subset_data), ")\n\n")
    next
  }
  
  # Correlation between mentions and absolute returns
  cor_val <- cor(subset_data$mentions, subset_data$abs_return)
  model <- lm(abs_return ~ mentions, data = subset_data)
  p_val <- summary(model)$coefficients["mentions", "Pr(>|t|)"]
  
  # Split by mention level
  low_mention <- subset_data[subset_data$mentions <= median(subset_data$mentions), ]
  high_mention <- subset_data[subset_data$mentions > median(subset_data$mentions), ]
  
  avg_low <- mean(low_mention$abs_return)
  avg_high <- mean(high_mention$abs_return)
  ratio <- avg_high / avg_low
  
  cat(cat_name, ":\n")
  cat("  Correlation (mentions vs |return|):", round(cor_val, 4), 
      ifelse(p_val < 0.05, " *SIGNIFICANT*", ""), "\n")
  cat("  Low-mention days: avg |return| =", round(avg_low, 3), "%\n")
  cat("  High-mention days: avg |return| =", round(avg_high, 3), "%\n")
  cat("  Ratio:", round(ratio, 2), "x\n\n")
  
  results <- rbind(results, data.frame(
    Category = cat_name,
    N = nrow(subset_data),
    Corr_Abs_Return = cor_val,
    P_Value = p_val,
    Avg_Move_Low_Mentions = avg_low,
    Avg_Move_High_Mentions = avg_high,
    Ratio = ratio
  ))
}

print(results)
write.csv(results, "plots/magnitude_results.csv", row.names = FALSE)

# VOLATILITY SPIKE DETECTION
cat("\nVOLATILITY SPIKE ANALYSIS\n\n")

# Define spike as top 25% of absolute returns
for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) < 20) next
  
  threshold <- quantile(subset_data$abs_return, 0.75)
  subset_data$is_spike <- subset_data$abs_return > threshold
  
  # Compare mentions on spike vs non-spike days
  mentions_spike <- mean(subset_data$mentions[subset_data$is_spike])
  mentions_normal <- mean(subset_data$mentions[!subset_data$is_spike])
  
  cat(cat_name, ":\n")
  cat("  Spike threshold:", round(threshold, 3), "%\n")
  cat("  Avg mentions on spike days:", round(mentions_spike, 2), "\n")
  cat("  Avg mentions on normal days:", round(mentions_normal, 2), "\n")
  cat("  Lift:", round(mentions_spike / mentions_normal, 2), "x\n\n")
}

# VISUALIZATION
if (!dir.exists("plots")) dir.create("plots")

png("plots/magnitude_analysis.png", width = 1400, height = 450)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 10) {
    plot(jitter(subset_data$mentions, 0.2), 
         subset_data$abs_return,
         pch = 16, col = rgb(0, 0, 0, 0.4),
         xlab = "Reddit Mentions", 
         ylab = "Absolute Return (%)",
         main = paste(cat_name, "\nr =", round(cor(subset_data$mentions, subset_data$abs_return), 3)))
    
    abline(lm(abs_return ~ mentions, data = subset_data), col = "red", lwd = 2)
  }
}

dev.off()
cat("Saved: plots/magnitude_analysis.png\n")