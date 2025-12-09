#' Clean text by removing URLs, punctuation, extra whitespace
#' @param text Character vector of text to clean
#' @return Character vector of cleaned text
clean_text <- function(text) {
  # Step 1: Remove URLs
  cleaned_text <- gsub("https?://[^ ]+", "", text)
  
  # Step 2: Remove punctuation except spaces
  cleaned_text <- gsub("[^a-zA-Z0-9 ]", "", cleaned_text)
  
  # Step 3: Replace multiple spaces with single space
  cleaned_text <- gsub(" +", " ", cleaned_text)
  
  # Step 4: Trim leading/trailing whitespace
  cleaned_text <- gsub("^ +| +$", "", cleaned_text)
  
  return(cleaned_text)
}


#' Extract ticker symbols from text using regex
#' @param text Character vector of text
#' @param whitelist Character vector of valid ticker symbols
#' @return Character vector of tickers found
extract_tickers <- function(text, whitelist) {
  # Step 1: Find all 2-5 uppercase letter patterns
  matches <- gregexpr("\\b[A-Z]{2,5}\\b", text)
  potential_tickers <- unlist(regmatches(text, matches))
  
  # Step 2: Filter to whitelisted tickers only
  tickers <- potential_tickers[potential_tickers %in% whitelist]
  
  # Step 3: Return unique tickers
  tickers <- unique(tickers)
  
  return(tickers)
}


#' Aggregate ticker mentions by date
#' @param df Data frame with columns: ticker, date
#' @return Data frame with columns: date, ticker, mentions
aggregate_mentions <- function(df) {
  # Step 1: Count mentions per ticker per date
  mention_counts <- aggregate(
    rep(1, nrow(df)),
    by = list(date = df$date, ticker = df$ticker),
    FUN = sum
  )
  
  # Step 2: Rename count column
  names(mention_counts)[3] <- "mentions"
  
  # Step 3: Sort by date then ticker
  mention_counts <- mention_counts[order(mention_counts$date, mention_counts$ticker), ]
  rownames(mention_counts) <- NULL
  
  return(mention_counts)
}


#' Compute daily log returns from price series
#' @param prices Numeric vector of prices
#' @return Numeric vector of log returns (length = length(prices) - 1)
compute_returns <- function(prices) {
  # Step 1: Calculate log returns: log(P_t / P_{t-1})
  n <- length(prices)
  returns <- c(NA, log(prices[2:n] / prices[1:(n-1)]))
  
  return(returns)
}


#' Compute volatility as absolute value of returns
#' @param returns Numeric vector of returns
#' @return Numeric vector of volatility measures
compute_volatility <- function(returns) {
  # Step 1: Take absolute value as volatility measure
  volatility <- abs(returns)
  
  return(volatility)
}


#' Bootstrap regression coefficients
#' @param data Data frame with columns: mentions, returns
#' @param n_boot Number of bootstrap samples (default 1000)
#' @return Numeric vector of bootstrapped beta coefficients
bootstrap_regression <- function(data, n_boot = 1000) {
  # Step 1: Create empty vector for results
  beta_boot <- numeric(n_boot)
  n_obs <- nrow(data)
  
  # Step 2: Bootstrap loop
  for (i in 1:n_boot) {
    boot_indices <- sample(1:n_obs, n_obs, replace = TRUE)  # Sample with replacement
    boot_data <- data[boot_indices, ]
    boot_model <- lm(returns ~ mentions, data = boot_data)  # Fit model
    beta_boot[i] <- coef(boot_model)[2]  # Extract coefficient
  }
  
  return(beta_boot)
}


#' Permutation test for regression coefficient
#' @param data Data frame with columns: mentions, returns
#' @param n_perm Number of permutations (default 1000)
#' @return List with observed beta, null distribution, p-value
permutation_test <- function(data, n_perm = 1000) {
  # Step 1: Fit original model
  original_model <- lm(returns ~ mentions, data = data)
  beta_observed <- coef(original_model)[2]
  
  # Step 2: Create null distribution
  null_dist <- numeric(n_perm)
  
  # Step 3: Permutation loop
  for (i in 1:n_perm) {
    shuffled_data <- data
    shuffled_data$returns <- sample(data$returns)  # Shuffle returns
    perm_model <- lm(returns ~ mentions, data = shuffled_data)
    null_dist[i] <- coef(perm_model)[2]
  }
  
  # Step 4: Compute p-value (two-sided)
  p_value <- mean(abs(null_dist) >= abs(beta_observed))
  
  return(list(
    beta_observed = beta_observed,
    null_distribution = null_dist,
    p_value = p_value
  ))
}


#' Create formatted plot with consistent style
#' @param ... Standard plot arguments
#' @param save_path Optional path to save plot
plot_styled <- function(..., save_path = NULL) {
  # Step 1: Open graphics device if saving
  if (!is.null(save_path)) {
    png(save_path, width = 800, height = 600)
  }
  
  # Step 2: Create plot with larger text
  plot(..., cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.3, las = 1)
  
  # Step 3: Add grid
  grid(col = "gray80", lty = "dotted")
  
  # Step 4: Close device if saving
  if (!is.null(save_path)) {
    dev.off()
    message("Plot saved to: ", save_path)
  }
}

#' Merge Reddit and stock data by date and ticker
#' @param reddit_df Data frame with date, ticker, mentions
#' @param stock_df Data frame with date, ticker, close_price, returns
#' @return Merged data frame
merge_reddit_stocks <- function(reddit_df, stock_df) {
  # Step 1: Ensure dates are in Date format
  reddit_df$date <- as.Date(reddit_df$date)
  stock_df$date <- as.Date(stock_df$date)
  
  # Step 2: Merge by date and ticker
  merged <- merge(reddit_df, stock_df, 
                  by = c("date", "ticker"), 
                  all.x = TRUE, all.y = FALSE)
  
  # Step 3: Sort by ticker and date
  merged <- merged[order(merged$ticker, merged$date), ]
  rownames(merged) <- NULL
  
  return(merged)
}
