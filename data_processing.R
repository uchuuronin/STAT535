# 03_data_processing.R
# Extract tickers from Reddit posts and merge with stock data
# Course connection: Text processing (regex), data merging


cat("Loading data...\n")
reddit_data <- readRDS("data/raw_reddit.rds")
stock_data <- readRDS("data/raw_stocks.rds")

cat("Reddit posts:", nrow(reddit_data), "\n")
cat("Stock observations:", nrow(stock_data), "\n")

cat("\nExtracting tickers from Reddit posts...\n")

# Ticker whitelist (from your stock data)
ticker_whitelist <- unique(stock_data$ticker)

false_positives <- c("NOW", "AI", "IT", "SO", "ON", "OR", "ARE", "ALL", "FOR", "BY", "GO", "DD", "TA", "PE", "CEO", "ETF", "USA", "IRA", "WISH","COST")

ticker_whitelist <- ticker_whitelist[!ticker_whitelist %in% false_positives]

cat("Using", length(ticker_whitelist), "valid tickers (after removing false positives)\n")

# Clean and combine title + selftext
clean_text <- function(text) {
  text <- toupper(text)  # Uppercase
  text <- gsub("[^A-Z0-9 ]", " ", text)  # Remove special chars
  text <- gsub("\\s+", " ", text)  # Collapse whitespace
  return(text)
}

reddit_data$full_text <- paste(reddit_data$title, reddit_data$selftext, sep = " ")
reddit_data$clean_text <- clean_text(reddit_data$full_text)

# Extract tickers using regex
ticker_pattern <- "\\b[A-Z]{2,5}\\b"

extract_tickers <- function(text, whitelist) {
  matches <- gregexpr(ticker_pattern, text)
  extracted <- regmatches(text, matches)
  tickers <- unique(unlist(extracted))
  valid_tickers <- tickers[tickers %in% whitelist]
  return(valid_tickers)
}

reddit_data$tickers_found <- lapply(
  reddit_data$clean_text,
  extract_tickers,
  whitelist = ticker_whitelist
)

cat("Expanding to ticker mentions...\n")

ticker_mentions <- list()

for (i in 1:nrow(reddit_data)) {
  tickers <- reddit_data$tickers_found[[i]]
  
  if (length(tickers) > 0) {
    for (ticker in tickers) {
      ticker_mentions[[length(ticker_mentions) + 1]] <- data.frame(
        date = reddit_data$date[i],
        ticker = ticker,
        score = reddit_data$score[i],
        stringsAsFactors = FALSE
      )
    }
  }
}

ticker_mentions_df <- do.call(rbind, ticker_mentions)
rownames(ticker_mentions_df) <- NULL

cat("\nTicker mentions by month:")
ticker_mentions_df$month <- format(ticker_mentions_df$date, "%Y-%m")
print(table(ticker_mentions_df$month))

cat("Aggregating mentions by date and ticker...\n")
ticker_mentions_df$count <- 1
ticker_counts <- aggregate(
  count ~ date + ticker,
  data = ticker_mentions_df,
  FUN = sum
)
names(ticker_counts)[3] <- "mentions"

ticker_counts <- ticker_counts[order(ticker_counts$date, ticker_counts$ticker), ]
rownames(ticker_counts) <- NULL

cat("\nTicker extraction summary:\n")
cat("Total ticker-day observations:", nrow(ticker_counts), "\n")
cat("Unique tickers found:", length(unique(ticker_counts$ticker)), "\n")

cat("\nTicker mentions over time:\n")
mentions_by_month <- aggregate(mentions  ~ format(date, "%Y-%m"), 
                               data = ticker_counts, FUN = sum)
names(mentions_by_month) <- c("month", "total_mentions")
print(mentions_by_month)


cat("\nMerging Reddit mentions with stock returns...\n")

# Merge on date and ticker
merged_data <- merge(
  ticker_counts,
  stock_data[, c("date", "ticker", "close", "returns")],
  by = c("date", "ticker"),
  all = FALSE  # Keep only rows where both Reddit and stock data exist
)

# Sort
merged_data <- merged_data[order(merged_data$date, merged_data$ticker), ]
rownames(merged_data) <- NULL

cat("Total observations:", nrow(merged_data), "\n")
cat("Unique tickers:", length(unique(merged_data$ticker)), "\n")
cat("Date range:", format(min(merged_data$date)), "to", format(max(merged_data$date)), "\n")

cat("\nMentions per day summary:\n")
print(summary(merged_data$mentions))

cat("\nReturns summary:\n")
print(summary(merged_data$returns))

cat("\nCorrelation between mentions and returns:\n")
# Remove NA returns for correlation
valid_rows <- !is.na(merged_data$returns)
if (sum(valid_rows) > 0) {
  cor_value <- cor(merged_data$mentions[valid_rows], 
                   merged_data$returns[valid_rows])
  cat("Correlation:", round(cor_value, 4), "\n")
}

cat("\nSample of merged data:\n")
print(head(merged_data, 15))

cat("\nTop mentioned tickers:\n")
top_mentions <- aggregate(mentions ~ ticker, data = merged_data, FUN = sum)
top_mentions <- top_mentions[order(-top_mentions$mentions), ]
print(head(top_mentions, 10))

output_file <- "data/combined_data.rds"
saveRDS(merged_data, file = output_file)
cat("\nMerged data saved to:", output_file, "\n")

# plotting mentions vs returns
if (!dir.exists("plots")) {
  dir.create("plots")
}

png("plots/mentions_vs_returns.png", width = 800, height = 600)
par(mar = c(4, 4, 3, 1))

valid_rows <- !is.na(merged_data$returns)
plot(merged_data$mentions[valid_rows], 
     merged_data$returns[valid_rows],
     pch = 16,
     col = rgb(0, 0, 1, 0.3),
     xlab = "Reddit Mentions",
     ylab = "Stock Returns",
     main = "Reddit Mentions vs Stock Returns")
abline(h = 0, col = "gray", lty = 2)
abline(lm(returns ~ mentions, data = merged_data[valid_rows, ]), 
       col = "red", lwd = 2)

dev.off()
cat("Saved plot to: plots/mentions_vs_returns.png\n")
