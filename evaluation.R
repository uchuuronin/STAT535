# Interpretation, diagnostics, and visualization of data

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

cat("\n\nWhat do the regression coefficients actually mean?\n\n")
categories <- c("Mega-cap", "Retail/Meme", "Semiconductor")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 5) {
    model <- lm(returns ~ mentions, data = subset_data)
    beta <- coef(model)["mentions"]
    
    cat(cat_name, ":\n")
    cat("β =", round(beta, 6), "\n")
    cat("Interpretation: 1 additional Reddit mention is associated with a", round(beta * 100, 4), "% change in stock return\n")
    cat("  Practical example: 10 more mentions might mean:", round(beta * 10 * 100, 3), "% return change\n")
    
    if (abs(beta * 10) > 0.01) {
      cat("\tEconomic significance: 10 mentions move price by", 
          round(abs(beta * 10 * 100), 2), "% - MEANINGFUL!\n")
    } else {
      cat("\tEconomic significance: Effect too small for practical trading\n")
    }
    cat("\n")
  }
}

cat("\n\nWhy is Retail/Meme not significant despite r=0.16?\n\n")
for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 5) {
    n <- nrow(subset_data)
    
    # Observed correlation
    r_obs <- cor(subset_data$mentions, subset_data$returns)
    
    # Critical r for significance at α=0.05, two-tailed
    # Using t-distribution: t = r * sqrt((n-2)/(1-r^2))
    # For significance, need |t| > t_crit
    t_crit <- qt(0.975, n - 2)
    
    # Minimum r needed for significance (approximation)
    r_min <- t_crit / sqrt(n - 2 + t_crit^2)
    
    cat(cat_name, ":\n")
    cat("  Sample size (n):", n, "\n")
    cat("  Observed correlation:", round(r_obs, 4), "\n")
    cat("  Minimum r for significance:", round(r_min, 4), "\n")
    
    if (abs(r_obs) < r_min) {
      cat(" \tObserved r is below threshold (underpowered)\n")
    } else {
      cat(" \tSample size is adequate to detect this effect\n")
    }
    cat("\n")
  }
}

cat("\n\nChecking assumptions for the significant finding (Mega-cap)\n\n")

mega_data <- data[data$category == "Mega-cap" & !is.na(data$category), ]
model_mega <- lm(returns ~ mentions, data = mega_data)

cat("Mega-cap regression diagnostics:\n")

# Residual analysis
residuals <- residuals(model_mega)
cat("  Residual mean:", round(mean(residuals), 6), "(should be ~0)\n")
cat("  Residual SD:", round(sd(residuals), 6), "\n")

# Check for normality (informal - Shapiro-Wilk)
shapiro_test <- shapiro.test(residuals)
cat("  Normality test (Shapiro-Wilk): p =", round(shapiro_test$p.value, 4))
if (shapiro_test$p.value > 0.05) {
  cat(" ✓ Normal\n")
} else {
  cat(" (non-normal, but OK for large n)\n")
}

# Check for outliers (standardized residuals)
std_resid <- rstandard(model_mega)
n_outliers <- sum(abs(std_resid) > 2)
cat("  Outliers (|std residual| > 2):", n_outliers, "out of", length(std_resid), "\n")

# Heteroskedasticity check (informal)
fitted_vals <- fitted(model_mega)
cor_resid_fitted <- cor(abs(residuals), fitted_vals)
cat("  Heteroskedasticity check: cor(|residuals|, fitted) =", round(cor_resid_fitted, 4))
if (abs(cor_resid_fitted) < 0.3) {
  cat(" ✓ OK\n")
} else {
  cat(" (possible heteroskedasticity)\n")
}

cat("\n")

cat("\n\nDo patterns change over time?\n\n")

# Split data into early vs late periods
median_date <- median(data$date)
data$period <- ifelse(data$date < median_date, "Early", "Late")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 10) {
    early <- subset_data[subset_data$period == "Early", ]
    late <- subset_data[subset_data$period == "Late", ]
    
    if (nrow(early) >= 5 && nrow(late) >= 5) {
      r_early <- cor(early$mentions, early$returns)
      r_late <- cor(late$mentions, late$returns)
      
      cat(cat_name, ":\n")
      cat("  Early period correlation:", round(r_early, 4), "\n")
      cat("  Late period correlation:", round(r_late, 4), "\n")
      
      if (sign(r_early) != sign(r_late)) {
        cat("Note: Correlation FLIPPED direction over time!\n")
      } else if (abs(r_late - r_early) > 0.2) {
        cat("Substantial change in correlation strength\n")
      } else {
        cat("Relatively stable over time\n")
      }
      cat("\n")
    }
  }
}

cat("\n\nDo results hold after removing extreme values?\n\n")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 10) {
    # Remove top/bottom 5% of returns
    q05 <- quantile(subset_data$returns, 0.05)
    q95 <- quantile(subset_data$returns, 0.95)
    trimmed_data <- subset_data[subset_data$returns > q05 & subset_data$returns < q95, ]
    
    r_original <- cor(subset_data$mentions, subset_data$returns)
    r_trimmed <- cor(trimmed_data$mentions, trimmed_data$returns)
    
    cat(cat_name, ":\n")
    cat("  Original correlation:", round(r_original, 4), "(n=", nrow(subset_data), ")\n")
    cat("  Trimmed correlation:", round(r_trimmed, 4), "(n=", nrow(trimmed_data), ")\n")
    
    if (abs(r_trimmed - r_original) < 0.05) {
      cat("ROBUST (results don't depend on outliers)\n")
    } else {
      cat("Results sensitive to extreme values\n")
    }
    cat("\n")
  }
}

cat("\n\nSummary:\n\n")

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

print(summary_table)

write.csv(summary_table, "plots/summary_statistics.csv", row.names = FALSE)
cat("\nSaved to: plots/summary_statistics.csv\n\n")

if (!dir.exists("plots")) dir.create("plots")

# PLOT 1: Regression by category with data density
cat("Creating visualization: Regression lines with confidence bands...\n")

png("plots/final_regression_by_category.png", width = 1400, height = 450)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1), cex.main = 1.3, cex.lab = 1.1)

colors <- c("Mega-cap" = "blue", "Retail/Meme" = "red", "Semiconductor" = "green")

for (cat_name in categories) {
  subset_data <- data[data$category == cat_name & !is.na(data$category), ]
  
  if (nrow(subset_data) >= 5) {
    model <- lm(returns ~ mentions, data = subset_data)
    
    # Convert to percentage
    y_vals <- subset_data$returns * 100
    x_vals <- subset_data$mentions
    
    # Zoom to 5th-95th percentile (less aggressive than 2-98)
    y_min <- quantile(y_vals, 0.05)
    y_max <- quantile(y_vals, 0.95)
    
    # Add padding
    y_range <- y_max - y_min
    y_min <- y_min - 0.1 * y_range
    y_max <- y_max + 0.1 * y_range
    
    # Plot with jitter
    plot(jitter(x_vals, amount = 0.1), 
         jitter(y_vals, amount = 0.1),
         pch = 16,
         cex = 1.2,
         col = rgb(0, 0, 0, 0.4),
         xlab = "Reddit Mentions",
         ylab = "Stock Return (%)",
         main = paste0(cat_name, 
                       "\nr = ", round(cor(x_vals, y_vals), 3),
                       ", p = ", round(summary(model)$coefficients["mentions", "Pr(>|t|)"], 3)),
         ylim = c(y_min, y_max),
         xlim = c(min(x_vals) - 0.5, max(x_vals) + 0.5))
    
    # Regression line (scale to percentage)
    abline(a = coef(model)[1] * 100, 
           b = coef(model)[2] * 100, 
           col = colors[cat_name], 
           lwd = 4)
    
    # Zero line
    abline(h = 0, col = "gray30", lty = 2, lwd = 2)
    
    # Sample size and significance indicator
    sig_star <- ifelse(summary(model)$coefficients["mentions", "Pr(>|t|)"] < 0.05, "*", "")
    text(max(x_vals) * 0.8, y_max * 0.85, 
         paste0("n = ", nrow(subset_data), sig_star), 
         cex = 1.3, font = 2)
  }
}

dev.off()
cat("Saved: plots/final_regression_by_category.png\n")

# PLOT 2: Time series for mega-cap (the significant one)
cat("Creating time series visualization...\n")

png("plots/final_timeseries_megacap.png", width = 1000, height = 600)
par(mar = c(5, 5, 4, 5))

mega_data <- data[data$category == "Mega-cap" & !is.na(data$category), ]
mega_agg <- aggregate(cbind(mentions, returns) ~ date, data = mega_data, FUN = sum)
mega_agg <- mega_agg[order(mega_agg$date), ]

# Plot mentions
plot(mega_agg$date, mega_agg$mentions,
     type = "h",
     col = "steelblue",
     lwd = 3,
     xlab = "Date",
     ylab = "Total Daily Mentions",
     main = "Mega-Cap Stocks: Reddit Mentions vs Returns Over Time")

# Add returns on second axis
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
