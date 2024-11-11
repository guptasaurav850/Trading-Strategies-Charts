# Load necessary libraries
library(quantmod)
library(TTR)  
library(PerformanceAnalytics)
library(ggplot2)

### Creating function for EMA Strategy ###

EMA_strategy <- function(symbol, n1 = 12, n2 = 26) {
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  ema_short <- EMA(Ad(stock), n1)
  ema_long <- EMA(Ad(stock), n2)
  
  cat("Applied EMA strategy with", n1, "and", n2, "periods.\n")
  
  # Plotting the Adj. Price with EMA lines
  plot(Ad(stock), main = paste(symbol, "EMA Strategy"), col = "blue", lwd = 2)
  lines(ema_short, col = "red", lwd = 2)
  lines(ema_long, col = "green", lwd = 2)
  
}

### Creating function for SMA Strategy ###
SMA_strategy <- function(symbol, n = 50) {
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  sma <- SMA(Ad(stock), n)
  
  cat("Applied SMA strategy with", n, "period.\n")
  
  # Plotting the Adj. Price with SMA line
  plot(Ad(stock), main = paste(symbol, "SMA Strategy"), col = "blue", lwd = 2)
  lines(sma, col = "orange", lwd = 2)
  
}

### Creating function for MACD Strategy ###
MACD_strategy <- function(symbol) {
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  macd <- MACD(Ad(stock), 12, 26, 9, type = "EMA")
  
  cat("Applied MACD strategy.\n")
  
  # Plotting the Adj. Price with MACD indicator
  plot(Ad(stock), main = paste(symbol, "MACD Strategy"), col = "blue", lwd = 2)
  plot(macd$macd, main = paste(symbol, "MACD"), col = "red", lwd = 2)
  lines(macd$signal, col = "green", lwd = 2)
}

### Creating function for RSI Strategy ###
RSI_strategy <- function(symbol, n = 14) {
  stock <- getSymbols(symbol, from = "2020-01-01", auto.assign = FALSE)
  rsi <- RSI(Ad(stock), n)
  
  cat("Applied RSI strategy with", n, "period.\n")
  
  # Plotting the Adj. Price with RSI Indicator
  plot(Ad(stock), main = paste(symbol, "RSI Strategy"), col = "blue", lwd = 2)
  plot(rsi, main = paste(symbol, "RSI"), col = "blue", lwd = 2)
}

# Creating function to prompt the user for strategy and execute it
trading_strategy_switch <- function() {
  # Input: Get user's preferred trading strategy
  cat("Choose a trading strategy: \n1. EMA\n2. SMA\n3. MACD\n4. RSI\n")
  choice <- as.integer(readline(prompt = "Enter your choice (1-4): "))
  
  # Input: Get stock ticker from user
  symbol <- readline(prompt = "Enter NIFTY 50 stock symbol (e.g., SBIN.NS, BHARTIARTL.NS): ")
  
  # Execute the corresponding strategy based on user input
  switch(choice,
         "1" = EMA_strategy(symbol),
         "2" = SMA_strategy(symbol),
         "3" = MACD_strategy(symbol),
         "4" = RSI_strategy(symbol),
         cat("Invalid choice. Please choose a number between 1 and 4.\n"))
}

# Run the trading strategy function
trading_strategy_switch()
ASHOKLEY.NS
