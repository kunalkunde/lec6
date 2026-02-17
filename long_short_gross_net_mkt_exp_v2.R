setwd("C:/Users/Kunal Kunde/OneDrive/Documents/SOM 660/Lec 6")
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(PerformanceAnalytics)
###################
prices_raw <- read_tsv(
  "2026.02.10.Daily prices for 32 assets past 5Y.txt",
  na = c("#N/A"),
  col_types = cols(.default = col_double())
)
prices_raw <- prices_raw %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30"))
prices_clean <- prices_raw %>%
  filter(
    rowSums(is.na(select(., -Date))) < ncol(.) - 1
  )
###################
price_to_returns <- function(prices_df) {
  prices_df %>%
    arrange(Date) %>%
    mutate(across(-Date, ~ (.x / lag(.x)) - 1))
}
###################
get_rebalance_indices <- function(dates, freq) {
  freq <- match.arg(freq, c("quarterly", "semiannual", "annual"))
  
  ep <- endpoints(
    xts(order.by = dates),
    on = switch(freq,
                quarterly = "quarters",
                semiannual = "years",
                annual = "years"),
    k = ifelse(freq == "semiannual", 2, 1)
  )
  
  ep[ep > 0]
}
###################
simulate_long_short_portfolio <- function(
    prices_clean,
    net_exposure,          # e.g. 0.00
    gross_exposure,        # e.g. 0.80
    long_exposure,         # e.g. 0.40
    n_long_stocks,         # e.g. 16
    rebalance_freq,        # quarterly/semiannual/annual
    accrual_rate_annual    # e.g. 0.06 (6%)
) {
  
  stopifnot(long_exposure <= gross_exposure)
  
  # Derived exposures
  short_exposure <- long_exposure - net_exposure
  
  # Convert prices to returns
  returns_df <- price_to_returns(prices_clean)
  dates <- returns_df$Date
  
  stock_cols <- colnames(returns_df)[2:(ncol(returns_df)-1)]
  index_col  <- "NIFTY Index"
  
  # Random long basket
  set.seed(123)
  long_assets <- sample(stock_cols, n_long_stocks, replace = FALSE)
  
  # Rebalance points
  rb_idx <- get_rebalance_indices(dates, rebalance_freq)
  rb_idx <- c(1, rb_idx)
  
  n_days <- nrow(returns_df)
  
  # --- STORAGE ---
  long_pos  <- matrix(0, n_days, n_long_stocks)
  colnames(long_pos) <- long_assets
  
  short_pos <- numeric(n_days)
  cash_mm   <- numeric(n_days)
  cash_fut  <- numeric(n_days)
  total_val <- numeric(n_days)
  
  # --- INITIALISATION (t=1) ---
  total_val[1] <- 100
  
  long_pos[1, ] <- (long_exposure * 100) / n_long_stocks
  short_pos[1]  <- - short_exposure * 100
  
  cash_mm[1]  <- (1 - long_exposure) * 100
  cash_fut[1] <- short_exposure * 100
  
  # --- SIMULATION LOOP ---
  prev_rb <- 1
  
  for (t in 2:n_days) {
    
    # --- Drift positions ---
    long_ret_today  <- as.numeric(returns_df[t, long_assets])
    index_ret_today <- returns_df[[index_col]][t]
    
    long_pos[t, ] <- long_pos[t-1, ] * (1 + long_ret_today)
    
    short_pos[t] <- short_pos[t-1] * (1 + index_ret_today)
    
    # Money-market accrual
    day_diff <- as.numeric(dates[t] - dates[t-1])
    cash_mm[t] <- cash_mm[t-1]*(1 + accrual_rate_annual)^(day_diff / 365)    

    # Futures offset stays constant
    cash_fut[t] <- cash_fut[t-1]
    
    total_val[t] <- sum(long_pos[t, ]) -(short_pos[t]+ cash_fut[t])  +
      cash_mm[t] 
    
    # --- Rebalance check ---
    if (t %in% rb_idx) {
      
      # Total portfolio value already marked-to-market
      TV <- total_val[t]
      
      # Target exposures
      target_long_total  <- long_exposure  * TV
      target_short_total <- - short_exposure * TV
      target_cash_offset <- short_exposure * TV
      
      # Reset positions
      long_pos[t, ] <- target_long_total / n_long_stocks
      short_pos[t]  <- target_short_total
      cash_fut[t]   <- target_cash_offset
      
      # Cash_MM becomes residual
      cash_mm[t] <- TV -
        sum(long_pos[t, ]) -
        short_pos[t] -
        cash_fut[t]
    }
  }
  
  # --- OUTPUT ---
  allocation_df <- data.frame(
    Date = dates,
    long_pos,
    Short_Index = short_pos,
    Cash_MM = cash_mm,
    Cash_Futures_Offset = cash_fut,
    Total_Portfolio_Value = total_val
  )
  
  list(
    allocations = allocation_df,
    long_assets = long_assets
  )
}
###################
result <- simulate_long_short_portfolio(
  prices_clean       = prices_clean,
  net_exposure       = 0.00,
  gross_exposure     = 0.80,
  long_exposure      = 0.40,
  n_long_stocks      = 16,
  rebalance_freq     = "quarterly",
  accrual_rate_annual = 0.06
)

head(result$allocations)
