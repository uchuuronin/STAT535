# Note: Reddit's API has rate limits. This script includes delays and User-Agent
# to comply with Reddit's API guidelines. If you hit 429 errors, wait 5-10 minutes.

# Load required libraries
library(jsonlite)  # For parsing JSON
library(httr)      # For HTTP requests with custom headers

# Source helper functions
source("utils.R")

# Subreddits to scrape
subreddits <- c("wallstreetbets", "stocks","investing","trading","stockmarket")

# Number of pages to fetch per subreddit (100 posts per page)
# More pages = more historical data
n_pages <- 25

# Output file path
output_file <- "data/raw_reddit.rds"


#' Fetch posts from a subreddit using Reddit's JSON API with pagination
#' @param subreddit Name of subreddit (without r/)
#' @param n_pages Number of pages to fetch (100 posts per page)
#' @return Data frame with post information
scrape_subreddit <- function(subreddit, n_pages = 2) {
  all_data <- list()
  after_token <- NULL
  
  # User-Agent required by Reddit API
  user_agent <- "R:reddit-stock-analysis:v1.0 (by /u/stats535project)"
  
  for (i in 1:n_pages) {
    url <- paste0("https://www.reddit.com/r/", subreddit, "/new.json?limit=100")
    
    if (!is.null(after_token)) {
      url <- paste0(url, "&after=", after_token)
    }
    
    # Make request with User-Agent header
    response <- GET(url, user_agent(user_agent))
    
    if (status_code(response) != 200) {
      warning("Failed to fetch page ", i, " for r/", subreddit, 
              ": HTTP ", status_code(response))
      break
    }
    
    raw_json <- content(response, "parsed", type = "application/json")
    posts_data <- raw_json$data$children
    
    if (length(posts_data) == 0) break  # No more posts
    
    # Extract data from list structure
    posts_df <- data.frame(
      subreddit = subreddit,
      title = sapply(posts_data, function(x) x$data$title),
      selftext = sapply(posts_data, function(x) x$data$selftext),
      created_utc = sapply(posts_data, function(x) x$data$created_utc),
      score = sapply(posts_data, function(x) x$data$score),
      num_comments = sapply(posts_data, function(x) x$data$num_comments),
      stringsAsFactors = FALSE
    )
    
    all_data[[i]] <- posts_df
    
    after_token <- raw_json$data$after
    if (is.null(after_token)) break  # No more pages available
    
    Sys.sleep(3)  # Longer delay between pages (3 seconds)
  }
  
  df <- do.call(rbind, all_data)
  df$datetime <- as.POSIXct(df$created_utc, origin = "1970-01-01")
  df$date <- as.Date(df$datetime)
  
  
  return(df)
}


# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Initialize empty list to store results
all_posts <- list()

# Loop through subreddits and scrape each one
for (sub in subreddits) {
  cat("Scraping r/", sub, "...\n", sep = "")
  
  # Scrape with error handling
  df <- tryCatch({
    scrape_subreddit(sub, n_pages)
  }, error = function(e) {
    cat("Error scraping r/", sub, ": ", e$message, "\n", sep = "")
    return(NULL)
  })
  
  # Store result if successful
  if (!is.null(df)) {
    all_posts[[sub]] <- df
  }
  
  # Delay between subreddits to avoid rate limits
  Sys.sleep(5)
}

# Combine all posts into single data frame
reddit_data <- do.call(rbind, all_posts)
rownames(reddit_data) <- NULL

# Print summary statistics
cat("Total posts:", nrow(reddit_data), "\n")
cat("Subreddits:", paste(unique(reddit_data$subreddit), collapse = ", "), "\n")
cat("Date range:", format(min(reddit_data$date)), "to", format(max(reddit_data$date)), "\n")
cat("Time span:", round(difftime(max(reddit_data$datetime), min(reddit_data$datetime), units = "hours"), 1), "hours\n")
cat("Posts per subreddit:\n")
print(table(reddit_data$subreddit))

cat("\nOldest post:", format(min(reddit_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Newest post:", format(max(reddit_data$datetime), "%Y-%m-%d %H:%M:%S"), "\n")
head(reddit_data)

# Check for missing values
cat("\nMissing values:\n")
print(colSums(is.na(reddit_data)))

# Save as RDS file for later use
saveRDS(reddit_data, file = output_file)
cat("\nData saved to:", output_file, "\n")