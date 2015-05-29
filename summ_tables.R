# FILE   : summ_tables.R
# PURPOSE: Create exportable .csvs comparing each of our variables.
# NOTES  : Written by Ethan Edwards.
#          Please run "read_stocks.R" first to create the "stocks" list!

# We have our list "stocks," which is very useful to begin with.
# But to make our code easier to run and store, we might want
# aggregate tables over all dates instead. Let's try to make one now.

# First, we need our list of 500 stock companies (from read_stocks.R).
# In addition, we need the earliest date and the latest date:

date.range <- matrix(date(1000),ncol=2)
for (i in 1:500) {
  date.range[i,1] <- min(stocks[[i]][,1])
  date.range[i,2] <- max(stocks[[i]][,1])
}

# This... doesn't work quite as we'd hope.
# But it does tell us one important thing: that stocks[[2]] has the
# complete range of dates. Thus we can use it for our rows:

date.range <- length(stocks[[2]][,1])

# And now, we create our empty matrices:

agg.open <- matrix(NA, nrow=date.range, ncol=500)
rownames(agg.open) <- as.character(stocks[[2]][,1])
colnames(agg.open) <- stock_names

agg.close <- matrix(NA, nrow=date.range, ncol=500)
rownames(agg.close) <- as.character(stocks[[2]][,1])
colnames(agg.close) <- stock_names

agg.hi <- matrix(NA, nrow=date.range, ncol=500)
rownames(agg.hi) <- as.character(stocks[[2]][,1])
colnames(agg.hi) <- stock_names

agg.lo <- matrix(NA, nrow=date.range, ncol=500)
rownames(agg.lo) <- as.character(stocks[[2]][,1])
colnames(agg.lo) <- stock_names

agg.vol <- matrix(NA, nrow=date.range, ncol=500)
rownames(agg.vol) <- as.character(stocks[[2]][,1])
colnames(agg.vol) <- stock_names

# Phew! They're all semi-large (8 Mb NA, 15 filled), but not terrible.
# We just need to fill out the data, now.

# fill.time <- proc.time()
# for (j in 1:500) {
#   i.lo <- which(rownames(agg.open) == as.character(min(stocks[[j]][,1])))
#   i.hi <- which(rownames(agg.open) == as.character(max(stocks[[j]][,1])))
#   agg.open[i.lo:i.hi, j] <- stocks[[j]][,2]
#   agg.close[i.lo:i.hi, j] <- stocks[[j]][,3]
#   agg.hi[i.lo:i.hi, j] <- stocks[[j]][,4]
#   agg.lo[i.lo:i.hi, j] <- stocks[[j]][,5]
#   agg.vol[i.lo:i.hi, j] <- stocks[[j]][,6]
# }
# proc.time() - fill.time

# THIS RUNS INTO A CRITICAL EROR: stock number 41, 'aph', is missing data!
# We find each missing piece of data systematically:
# 
# mia.41 <- setdiff(rownames(agg.open), as.character(stocks[[41]][,1]))
# which(rownames(agg.open) == mia.41)
# 
# mia.62 <- setdiff(rownames(agg.open), as.character(stocks[[62]][,1]))
# which(rownames(agg.open) == mia.62)
# 
# mia.141 <- setdiff(rownames(agg.open), as.character(stocks[[141]][,1]))
# which(rownames(agg.open) == mia.141)
# 
# mia.145 <- setdiff(rownames(agg.open), as.character(stocks[[141]][,1]))
# which(rownames(agg.open) == mia.145)
# 
# fill.time <- proc.time()
# for (j in c(1:40,42:61, 63:140, 142:144, 146:170, 172:500)) {
#   i.lo <- which(rownames(agg.open) == as.character(min(stocks[[j]][,1])))
#   i.hi <- which(rownames(agg.open) == as.character(max(stocks[[j]][,1])))
#   agg.open[i.lo:i.hi, j] <- stocks[[j]][,2]
#   agg.close[i.lo:i.hi, j] <- stocks[[j]][,3]
#   agg.hi[i.lo:i.hi, j] <- stocks[[j]][,4]
#   agg.lo[i.lo:i.hi, j] <- stocks[[j]][,5]
#   agg.vol[i.lo:i.hi, j] <- stocks[[j]][,6]
# }
# proc.time() - fill.time
#
# Or we don't, because this is taking an immense amount of time. Many rows are incomplete,
# and specifying each would take more computational overhead than just programming around it.
# Assuming there's only one piece of missing data per:
# 
# fill.time <- proc.time()
# for (j in 1:500) {
#   i.lo <- which(rownames(agg.open) == as.character(min(stocks[[j]][,1])))
#   i.hi <- which(rownames(agg.open) == as.character(max(stocks[[j]][,1])))
#   miss <- setdiff(rownames(agg.open[i.lo:i.hi,]), as.character(stocks[[j]][,1]))
#   skip <- which(rownames(agg.open) == miss)
#   
#   if (length(skip) > 0) {
#     agg.open[c(i.lo:(skip-1), (skip+1):i.hi), j] <- stocks[[j]][,2]
#     agg.close[c(i.lo:(skip-1), (skip+1):i.hi), j] <- stocks[[j]][,3]
#     agg.hi[c(i.lo:(skip-1), (skip+1):i.hi), j] <- stocks[[j]][,4]
#     agg.lo[c(i.lo:(skip-1), (skip+1):i.hi), j] <- stocks[[j]][,5]
#     agg.vol[c(i.lo:(skip-1), (skip+1):i.hi), j] <- stocks[[j]][,6]    
#   } else {
#     agg.open[i.lo:i.hi, j] <- stocks[[j]][,2]
#     agg.close[i.lo:i.hi, j] <- stocks[[j]][,3]
#     agg.hi[i.lo:i.hi, j] <- stocks[[j]][,4]
#     agg.lo[i.lo:i.hi, j] <- stocks[[j]][,5]
#     agg.vol[i.lo:i.hi, j] <- stocks[[j]][,6]
#   }
# }
# proc.time() - fill.time
#
# This is ALSO useless, as at 145 it branches into multiple problems.
# The fix should be easy, though:

fill.time <- proc.time()
for (j in 1:500) {
  i.lo <- which(rownames(agg.open) == as.character(min(stocks[[j]][,1])))
  i.hi <- which(rownames(agg.open) == as.character(max(stocks[[j]][,1])))
  miss <- setdiff(rownames(agg.open[i.lo:i.hi,]), as.character(stocks[[j]][,1]))
  skip <- numeric(length(miss))
  for (i in 1:length(miss)) {
    skip[i] <- which(rownames(agg.open) == miss[i])
  }
  i.new <- setdiff(i.lo:i.hi, skip)
  
  agg.open[i.new, j] <- stocks[[j]][,2]
  agg.close[i.new, j] <- stocks[[j]][,3]
  agg.hi[i.new, j] <- stocks[[j]][,4]
  agg.lo[i.new, j] <- stocks[[j]][,5]
  agg.vol[i.new, j] <- stocks[[j]][,6]
}
proc.time() - fill.time

write.csv(agg.open, "agg_open.csv")
write.csv(agg.close, "agg_close.csv")
write.csv(agg.hi, "agg_hi.csv")
write.csv(agg.lo, "agg_lo.csv")
write.csv(agg.vol, "agg_vol.csv")

# And with that, we have five aggregated csvs of all the data.