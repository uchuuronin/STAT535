# evaluation.R

merged_data <- readRDS("data/combined_data.rds")
data <- merged_data[!is.na(merged_data$returns), ]

mega_cap <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG")
retail_meme <- c("PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN")
semiconductor <- c("NVDA", "AMD", "AVGO", "INTC", "MU","TSM", "QCOM", "ARM")

data$category <- NA
data$category[data$ticker %in% mega_cap] <- "Mega-cap"
data$category[data$ticker %in% retail_meme] <- "Retail/Meme"
data$category[data$ticker %in% semiconductor] <- "Semiconductor"

cat("Total observations:", nrow(data), "\n")
cat("Date range:", format(min(data$date)), "to", format(max(data$date)), "\n\n")

categories <- c("Mega-cap", "Retail/Meme", "Semiconductor")

# Summary table
summary_table <- data.frame(
  Category = character(),
  n = integer(),
  Mean_Mentions = numeric(),
  Mean_Return_pct = numeric(),
  Correlation = numeric(),
  Beta = numeric(),
  SE = numeric(),
  t_stat = numeric(),
  p_value = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 5) {
    model <- lm(returns ~ mentions, data = subset_data)
    summ <- summary(model)
    
    summary_table <- rbind(summary_table, data.frame(
      Category = cat_name,
      n = nrow(subset_data),
      Mean_Mentions = mean(subset_data$mentions),
      Mean_Return_pct = mean(subset_data$returns) * 100,
      Correlation = cor(subset_data$mentions, subset_data$returns),
      Beta = coef(model)["mentions"],
      SE = summ$coefficients["mentions", "Std. Error"],
      t_stat = summ$coefficients["mentions", "t value"],
      p_value = summ$coefficients["mentions", "Pr(>|t|)"],
      R_squared = summ$r.squared
    ))
  }
}

cat("\nSummary Statistics:\n")
print(summary_table)

write.csv(summary_table, "plots/summary_statistics.csv", row.names = FALSE)
cat("\nSaved to: plots/summary_statistics.csv\n\n")

if (!dir.exists("plots")) dir.create("plots")

# PLOT 1: Regression by category
cat("Creating regression plots...\n")

png("plots/final_regression_by_category.png", width = 1400, height = 450)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), cex.main = 1.3, cex.lab = 1.1)

colors <- c("Mega-cap" = "blue", "Retail/Meme" = "red", "Semiconductor" = "green")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 5) {
    model <- lm(returns ~ mentions, data = subset_data)
    
    y_vals <- subset_data$returns * 100
    x_vals <- subset_data$mentions
    
    y_min <- quantile(y_vals, 0.05)
    y_max <- quantile(y_vals, 0.95)
    y_range <- y_max - y_min
    y_min <- y_min - 0.1 * y_range
    y_max <- y_max + 0.1 * y_range
    
    plot(jitter(x_vals, amount = 0.1), 
         jitter(y_vals, amount = 0.1),
         pch = 16,
         cex = 1.2,
         col = rgb(0, 0, 0, 0.4),
         xlab = "Reddit Mentions",
         ylab = "Stock Return (%)",
         main = paste0(cat_name, " (n=", nrow(subset_data), ")\n",
                       "r = ", round(cor(x_vals, y_vals), 3)),
         ylim = c(y_min, y_max),
         xlim = c(min(x_vals) - 0.5, max(x_vals) + 0.5))
    
    abline(a = coef(model)[1] * 100, 
           b = coef(model)[2] * 100, 
           col = colors[cat_name], 
           lwd = 4)
    
    abline(h = 0, col = "gray30", lty = 2, lwd = 2)
    
    sig_star <- ifelse(summary(model)$coefficients["mentions", "Pr(>|t|)"] < 0.05, "*", "")
    text(max(x_vals) * 0.8, y_max * 0.85, 
         paste0("p = ", round(summary(model)$coefficients["mentions", "Pr(>|t|)"], 3), sig_star), 
         cex = 1.3, font = 2)
  }
}

dev.off()
cat("Saved: plots/final_regression_by_category.png\n")

# PLOT 2: Time series for mega-cap
cat("Creating time series visualization...\n")

png("plots/final_timeseries_megacap.png", width = 1000, height = 600)
par(mar = c(5, 5, 4, 5))

mega_data <- data[data$category == "Mega-cap" & !is.na(data$category), ]
mega_agg <- aggregate(cbind(mentions, returns) ~ date, data = mega_data, FUN = sum)
mega_agg <- mega_agg[order(mega_agg$date), ]

plot(mega_agg$date, mega_agg$mentions,
     type = "h",
     col = "steelblue",
     lwd = 3,
     xlab = "Date",
     ylab = "Total Daily Mentions",
     main = "Mega-Cap Stocks: Reddit Mentions vs Returns Over Time")

par(new = TRUE)
plot(mega_agg$date, mega_agg$returns * 100,
     type = "l",
     col = "darkred",
     lwd = 2,
     axes = FALSE,
     xlab = "",
     ylab = "")

axis(4)
mtext("Daily Return (%)", side = 4, line = 3)

legend("topleft",
       legend = c("Reddit Mentions", "Returns"),
       col = c("steelblue", "darkred"),
       lwd = c(3, 2),
       bty = "n")

dev.off()
cat("Saved: plots/final_timeseries_megacap.png\n")

# PLOT 3: Diagnostic plot for mega-cap
cat("Creating diagnostic plot...\n")

png("plots/final_diagnostics_megacap.png", width = 800, height = 800)
par(mfrow = c(2, 2))

mega_data <- data[data$category == "Mega-cap" & !is.na(data$category), ]
model_mega <- lm(returns ~ mentions, data = mega_data)

plot(model_mega)

dev.off()
cat("Saved: plots/final_diagnostics_megacap.png\n")