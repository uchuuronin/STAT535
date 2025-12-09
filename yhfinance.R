# Fetch historical stock data from Yahoo Finance
# Uses quantmod package (will auto-install if not present)

# Load Reddit data to determine date range
reddit_data <- readRDS("data/raw_reddit.rds")

# Get date range (with 1-day buffer on each end)
start_date <- min(reddit_data$date) - 1
end_date <- max(reddit_data$date) + 1

cat("Reddit date range:", format(start_date), "to", format(end_date), "\n")

# Stock tickers to fetch
tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NFLX", "ORCL", "GOOG", # Mega-cap
             "PLTR", "HOOD", "SOFI", "GME", "RIVN", "RBLX", "COIN", # Retail/Meme
             "NVDA", "AMD", "AVGO", "INTC", "MU", "TSM", "QCOM", "ARM", # Semiconductor
             "SPY", "QQQ", "VOO", "VTI")         

# Output file
output_file <- "data/raw_stocks.rds"

# Load required library
# Try quantmod first (better Yahoo Finance integration)
if (!require("quantmod", quietly = TRUE)) {
  cat("Installing quantmod package...\n")
  install.packages("quantmod", quiet = TRUE)
  library(quantmod)
} else {
  library(quantmod)
}

#' Fetch stock data using quantmod
#' @param ticker Stock ticker symbol
#' @param start_date Start date (Date object)
#' @param end_date End date (Date object)
#' @return Data frame with date, ticker, close price
fetch_yahoo_quantmod <- function(ticker, start_date, end_date) {
  # Fetch data using quantmod
  data <- tryCatch({
    getSymbols(ticker, 
               from = start_date, 
               to = end_date, 
               auto.assign = FALSE,
               src = "yahoo")
  }, error = function(e) {
    stop("Failed to fetch ", ticker, call. = FALSE)
  })
  
  if (is.null(data) || nrow(data) == 0) {
    stop("No data returned for ", ticker, call. = FALSE)
  }
  
  # Extract close prices
  df <- data.frame(
    date = index(data),
    ticker = ticker,
    close = as.numeric(Cl(data)),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

#' Compute daily returns from price vector
#' @param prices Vector of prices
#' @return Vector of returns (first value is NA)
compute_returns <- function(prices) {
  returns <- c(NA, diff(log(prices)))  # Log returns
  return(returns)
}


if (!dir.exists("data")) {
  dir.create("data")
}

all_stocks <- list()

for (t in tickers) {
  cat("Fetching", t, "...\n")
  
  df <- tryCatch({
    fetch_yahoo_quantmod(t, start_date, end_date)
  }, error = function(e) {
    cat("Error fetching", t, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(df)) {
    all_stocks[[t]] <- df
  }
  
  Sys.sleep(1)  # Brief delay between requests
}

stock_data <- do.call(rbind, all_stocks)
rownames(stock_data) <- NULL

if (nrow(stock_data) == 0) {
  stop("No stock data was successfully fetched. Check your internet connection and ticker symbols.")
}


stock_data <- stock_data[order(stock_data$ticker, stock_data$date), ]
stock_data$returns <- NA

for (t in unique(stock_data$ticker)) {
  idx <- stock_data$ticker == t
  prices <- stock_data$close[idx]
  stock_data$returns[idx] <- compute_returns(prices)
}

cat("Total observations:", nrow(stock_data), "\n")
cat("Tickers:", paste(unique(stock_data$ticker), collapse = ", "), "\n")
cat("Date range:", format(min(stock_data$date)), "to", format(max(stock_data$date)), "\n")
cat("Observations per ticker:\n")
print(table(stock_data$ticker))

cat("\nPrice summary:\n")
print(summary(stock_data$close))

cat("\nReturn summary:\n")
print(summary(stock_data$returns))

head(stock_data, 10)

cat("\nMissing values:\n")
print(colSums(is.na(stock_data)))

saveRDS(stock_data, file = output_file)
cat("\nData saved to:", output_file, "\n")