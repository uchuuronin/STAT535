# Reddit Stock Analysis Pipeline

Statistical analysis examining the relationship between Reddit ticker discussion volume and stock market movements. 
Made for UMass Amherst Graduate Course STAT 535 Final Project

## Project Structure

### Data Collection
- **`reddit_scraper.R`** - Scrape Reddit posts from financial subreddits (wallstreetbets, stocks, etc.) using JSON API
- **`yhfinance.R`** - Fetch historical stock prices from Yahoo Finance for target tickers

### Data Processing
- **`data_processing.R`** - Extract ticker mentions using regex, merge with stock data, create combined dataset

### Exploratory (Running optional)
- **`reddit_ticker_mentions.R`** - Discover which tickers are mentioned (run once for exploration)

### Analysis Files (Run in Any Order)
- **`evaluation.R`** - Generate summary statistics and regression plots by stock category
- **`volatility_analysis.R`** - Analyze relationship between mentions and price movement magnitude (not direction)
- **`prediction.R`** - Train/test split validation. Also explores general lagged behaviour
- **`causality_check.R`** - Formal Granger causality test: do mentions predict returns (and vice versa)?

### Utilities
- **`utils.R`** - Helper functions for text processing, regression, and plotting


## Execution Order
```r
# 1. Data Collection (run once or when updating data)
source("reddit_scraper.R")     
source("yhfinance.R")           

# 2. Data Processing (required)
source("data_processing.R")      

# 3. Analysis (run any/all in any order)
source("evaluation.R")           # Basic stats + plots
source("volatility_analysis.R") # Magnitude analysis
source("prediction.R")           # Predictive validation
source("causality_check.R")      # Granger causality

# Optional: Ticker discovery (exploratory only)
source("reddit_ticker_mentions.R")
```