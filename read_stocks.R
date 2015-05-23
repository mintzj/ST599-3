# Load CSVs
# From project 2

#  Set your folder directory
#folders_path <- "Z:/ST599/Project3/stock/quantquote_daily_sp500_83986/daily"
folders_path <- "T:/Statistics/Share/mintzj/stock/quantquote_daily_sp500_83986/daily"
#folders_path <- "Z:/ST599/Project3/stock"


files <- list.files(path = folders_path, full.names = F)
stock_names <- sub(pattern = "^.*_([a-z]+)\\..+", replacement = "\\1", x = files, perl = T)

#  We think the data is 
#  Date, Time, Open, High, Low, Close, Volume

load_folder <- function(folders_path, n_stocks = 500){
  files <- list.files(path = folders_path, full.names = T)
  column_names <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
  stock_names <- sub(pattern = "^.*_([a-z]+)\\..+", replacement = "\\1", x = files, perl = T)
  n_files <- length(files)
  
  # Create a list to hold the stock data
  stocks <- vector("list", n_files)
  
  for (i in 1:n_files){
    stocks[[i]] <- read.csv(file = files[i], header = F)
    names(stocks[[i]]) <- column_names
  }
 
  names(stocks) <- stock_names
  return(stocks)

}


stocks <- load_folder(folders_path)
View(stocks$abt)

stock_names <- sub(pattern = "^.*_([a-z]+)\\..+", replacement = "\\1", x = files, perl = T)
head(stock_names, 100)
View(stocks[[1]])
