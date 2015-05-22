# Load CSVs
# From project 2

#  Set your folder directory
folders_path <- "Z:/ST599/Project3/stock/quantquote_daily_sp500_83986/daily"
folders_path <- "Z:/ST599/Project3/stock"


files <- list.files(path = folders_path, full.names = F)
stock_names <- sub(pattern = "^.*_([a-z]+)\\..+", replacement = "\\1", x = files, perl = T)


load_folder <- function(folders_path, n_stocks = 500){
  files <- list.files(path = folders_path, full.names = T)
  stock_names <- sub(pattern = "^.*_([a-z]+)\\..+", replacement = "\\1", x = files, perl = T)
  n_files <- length(files)
  
  # Create a list to hold the stock data
  stocks <- vector("list", n_files)
  
  for (i in 1:n_files){
    stocks[[i]] <- read.csv(file = files[i])
  }
  return(stocks)
}

stock_names <- sub(pattern = "^.*_([a-z]+)\\..+", replacement = "\\1", x = files, perl = T)
head(stock_names, 100)
View(stocks[[1]])

stocks <- load_folder(folders_path = folders_path)

# Grab the folder names and paths.
folders.path <- list.files(path = folders_path, full.names = T)
folders.name <- list.files(path = folders_path, full.names = F)


#  Load up n images from specified folder.
images <- load_folder(folders.path[20], n_images = 9)
plot_images(images = images, folder_name = folders.name[20])

#image(images[[6]])
#folders.name[15]