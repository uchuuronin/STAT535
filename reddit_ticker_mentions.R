# 03a_discover_tickers_v2.R
# Check which real stock tickers are mentioned in Reddit data
# Uses curated list of commonly discussed stocks

real_tickers <- c(
  # Mega-cap tech (FAANG+)
  "AAPL", "MSFT", "GOOGL", "GOOG", "AMZN", "META", "NFLX", "TSLA",
  
  # Semiconductors
  "NVDA", "AMD", "INTC", "TSM", "AVGO", "QCOM", "MU", "AMAT", "LRCX", 
  "KLAC", "SMCI", "ARM", "MRVL",
  
  # Other mega-caps
  "JPM", "BAC", "WFC", "C", "GS", "MS",  # Banks
  "JNJ", "UNH", "PFE", "ABBV", "TMO",    # Healthcare
  "XOM", "CVX", "COP",                    # Energy
  "WMT", "HD", "COST", "TGT",             # Retail
  "V", "MA", "AXP",                       # Payments
  
  # Popular tech/growth
  "CRM", "ORCL", "ADBE", "CSCO", "NOW", "SNOW", "PLTR", "PANW",
  "SHOP", "SQ", "PYPL", "COIN", "RBLX", "UBER", "ABNB", "DASH",
  
  # Meme stocks
  "GME", "AMC", "BB", "BBBY", "NOK", "WISH", "CLOV", "SPCE",
  
  # EV/Auto
  "TSLA", "F", "GM", "RIVN", "LCID", "NIO", "XPEV", "LI",
  
  # Aerospace/Defense  
  "BA", "LMT", "RTX", "NOC", "GD",
  
  # Communication/Media
  "DIS", "NFLX", "CMCSA", "T", "VZ", "TMUS",
  
  # Cloud/Software
  "SNOW", "DDOG", "NET", "CRWD", "ZS", "OKTA", "MDB",
  
  # Fintech
  "SOFI", "HOOD", "AFRM", "UPST",
  
  # AI/Tech hype
  "NVDA", "AMD", "PLTR", "AI", "SMCI", "ARM",
  
  # ETFs (commonly discussed)
  "SPY", "QQQ", "IWM", "DIA", "VOO", "VTI", "ARKK", 
  "TQQQ", "SQQQ", "SPXL", "UPRO", "UVXY", "VXX"
)

# Remove duplicates
real_tickers <- unique(real_tickers)

cat("Checking", length(real_tickers), "real stock tickers against Reddit data...\n")

reddit_data <- readRDS("data/raw_reddit.rds")

clean_text <- function(text) {
  text <- toupper(text)
  text <- gsub("[^A-Z0-9 ]", " ", text)
  text <- gsub("\\s+", " ", text)
  return(text)
}

reddit_data$full_text <- paste(reddit_data$title, reddit_data$selftext, sep = " ")
reddit_data$clean_text <- clean_text(reddit_data$full_text)

cat("Counting mentions...\n")

ticker_counts <- data.frame(
  ticker = character(),
  mentions = integer(),
  stringsAsFactors = FALSE
)

for (ticker in real_tickers) {
  # Use word boundary regex to avoid partial matches
  pattern <- paste0("\\b", ticker, "\\b")
  
  count <- sum(grepl(pattern, reddit_data$clean_text))
  
  if (count > 0) {
    ticker_counts <- rbind(ticker_counts,
                          data.frame(ticker = ticker,
                                    mentions = count,
                                    stringsAsFactors = FALSE))
  }
}

ticker_counts <- ticker_counts[order(-ticker_counts$mentions), ]


categorize_ticker <- function(ticker) {
  mega_cap_tech <- c("AAPL", "MSFT", "GOOGL", "GOOG", "AMZN", "META", "NFLX")
  mega_cap_other <- c("TSLA", "NVDA", "JPM", "BAC", "JNJ", "UNH", "V", "MA", "WMT", "XOM")
  
  meme <- c("GME", "AMC", "BB", "BBBY", "PLTR", "WISH", "CLOV", "SPCE", "NOK")
  
  semiconductor <- c("NVDA", "AMD", "INTC", "TSM", "AVGO", "QCOM", "MU", "AMAT", 
                    "LRCX", "KLAC", "SMCI", "ARM", "MRVL")
  
  etf <- c("SPY", "QQQ", "IWM", "DIA", "VOO", "VTI", "ARKK", "TQQQ", "SQQQ", 
           "SPXL", "UPRO", "UVXY", "VXX")
  
  fintech <- c("SQ", "PYPL", "COIN", "SOFI", "HOOD", "AFRM", "UPST")
  
  if (ticker %in% mega_cap_tech) return("Mega-cap Tech")
  if (ticker %in% mega_cap_other) return("Mega-cap Other")
  if (ticker %in% meme) return("Meme/Retail")
  if (ticker %in% semiconductor) return("Semiconductor")
  if (ticker %in% etf) return("ETF")
  if (ticker %in% fintech) return("Fintech")
  return("Mid-tier")
}

ticker_counts$category <- sapply(ticker_counts$ticker, categorize_ticker)

print(ticker_counts)

category_summary <- aggregate(mentions ~ category, data = ticker_counts, FUN = sum)
category_summary$num_tickers <- table(ticker_counts$category)[category_summary$category]
category_summary <- category_summary[order(-category_summary$mentions), ]
print(category_summary)


# Tier 1: Highly mentioned (>= 5 mentions)
tier1 <- ticker_counts[ticker_counts$mentions >= 5, ]
cat("\nTier 1: Highly mentioned (>=5 mentions):", nrow(tier1), "tickers\n")
if (nrow(tier1) > 0) {
  cat("Tickers:", paste(tier1$ticker, collapse = ", "), "\n")
}

# Tier 2: Moderately mentioned (3-4 mentions)
tier2 <- ticker_counts[ticker_counts$mentions >= 3 & ticker_counts$mentions < 5, ]
cat("\nTier 2: Moderately mentioned (3-4 mentions):", nrow(tier2), "tickers\n")
if (nrow(tier2) > 0) {
  cat("Tickers:", paste(tier2$ticker, collapse = ", "), "\n")
}

# Tier 3: Low mentions (1-2 mentions)
tier3 <- ticker_counts[ticker_counts$mentions < 3, ]
cat("\nTier 3: Low mentions (1-2 mentions):", nrow(tier3), "tickers\n")
if (nrow(tier3) > 0) {
  cat("Tickers:", paste(tier3$ticker, collapse = ", "), "\n")
}

saveRDS(ticker_counts, "data/ticker_analysis.rds")
write.csv(ticker_counts, "data/ticker_analysis.csv", row.names = FALSE)