# FILE   : arima_forecast.R
# PURPOSE: Using our stock data, use Holt-Winters to forecast on our stock.
# NOTES  : Writen by Ethan Edwards, using Yijun's earlier work as a basis.

library(ggplot2)
library(nlme)

aapl.ts <- ts(stocks$aapl$Close, frequency=252, start = c(1998, 1))
plot(aapl.ts)
plot(log(aapl.ts))
plot(decompose(log(aapl.ts)))

acf(aapl.ts)
acf(log(aapl.ts))
pacf(aapl.ts)
pacf(log(aapl.ts))

# PACF spike only at term 1, ACF spikes descending slowly from 1.
# This is almost certainly an AR(1) series of difference zero.

AIC(arima(aapl.ts, order=c(1,0,0), method="ML"),
    arima(aapl.ts, order=c(0,0,1), method="ML"),
    arima(aapl.ts, order=c(1,0,1), method="ML"))

# acf(resid(arima(aapl.ts, order=c(1,0,0), method="ML", seasonal=c(1,0,0))))

# As expected, our ACF for an AR(1) series is the best.
# Using this as our fit, we can further work on season

ar.fit <- arima(aapl.ts, order=c(1,0,0), method="ML")
acf(resid(ar.fit))
pacf(resid(ar.fit))

# Use Holt-Winters to forecast the time series:

aapl.hw <- HoltWinters(aapl.ts)
predict(aapl.hw, 50)

# This seems low; however, it's using the entire period of time to predict performance,
# including the nearly six year startup period. What if we remove that?

aapl.cut <- ts(aapl.ts[(3926/2 + 1):3926], frequency=252, start = c(2005.5, 1))
aapl.hw.cut <- HoltWinters(aapl.cut)
plot(predict(aapl.hw.cut, 500))

# Not much better, honestly; our forecasting seems to be doing decently enough.
# Now instead, let's focus on regression:

aapl.ts <- ts(stocks$aapl$Close, frequency=252, start = c(1998, 1))
abc.ts <- ts(stocks$abc$Close, frequency=252, start = c(1998, 1))

aapl.sh.ts <- ts(stocks$aapl$Close[1667:3926], frequency=252, start=c(2004.6,1))
goog.ts <- ts(stocks$goog$Close, frequency=252, start=c(2007, 1))

aapl.lg <- log(aapl.sh.ts)
goog.lg <- log(goog.ts)

qplot(y=goog.lg, x=aapl.lg)

log.fit <- lm(aapl.lg ~ goog.lg)
acf(log.fit$res)
pacf(log.fit$res)

# Once again, these seem to be AR(1) residuals.
# Testing:

test.100 <- arima(log.fit$res, order = c(1,0,0))
acf(residuals(test.100))
pacf(residuals(test.100))

# Basically perfect in appearance.

pred.dat <- data.frame(cbind(as.numeric(aapl.sh.ts), as.numeric(goog.ts)))
pred.dat.lg <- data.frame(cbind(as.numeric(aapl.lg), as.numeric(goog.lg)))
names(pred.dat) <- c("AAPL","GOOG")
names(pred.dat.lg) <- c("AAPL","GOOG")


gls_fit.100.lg <- gls(AAPL ~ GOOG, data=pred.dat.lg, correlation = corARMA(p=1), method="ML")
acf(residuals(gls_fit.100, type="normalized"))
pacf(residuals(gls_fit.100, type="normalized"))

# Interesting; PACF possibly hints at mild MA seasonality every 12? Look into this!

gls_fit.100 <- gls(AAPL ~ GOOG, data=pred.dat, correlation = corARMA(p=1), method="ML")
acf(residuals(gls_fit.100, type="normalized"))
pacf(residuals(gls_fit.100, type="normalized"))

# Non-log variant gives a much uglier PACF, with no real hint towards seasonality!

qqnorm(residuals(gls_fit.100.lg, type="normalized"))
# Not ideal, but at last distributed decently-ish.
qplot(y=residuals(gls_fit.100.lg, type="normalized"), x=1:2260, geom="line")
# also not entirely ideal, but, well.

# Let's give a week or two of the most recent GOOG prices (5/13/15 through 6/1/15):

goog.new <-c(533.99, 532.11, 539.78, 539.79, 532.32, 540.11, 542.51, 539.27, 537.36, 532.30, 533.85, 538.40, 529.62)
pred.new <- data.frame(log(goog.new))
names(pred.new) <- "GOOG"

exp(predict(gls_fit.100.lg, pred.new))

# Massively lower than we'd expect; the semi-stationary responses of Holt-Winters are much closer.
# Is it just an issue of GOOG falling, or something more sinister?

# Let's try massively overfitting, and regressing on *each other stock* - if our computer can handle it.

agg.open <- read.csv("agg_open.csv", header=T, row.names=1)

fit.time <- proc.time()
gls_fit.huge <- gls(aapl ~ ., data=agg.open, correlation = corARMA(p=1), method="ML")
proc.time() - fit.time

# This fails where we'd expect - in dealing with the NAs.
# Given that we're fitting a linear regression, replacing all NAs with zero should work here.
# (NOTE: we normally wouldn't do that)

agg.open[is.na(agg.open)] <- 0

# Note: rather than download 500 instances of new stock data to provide our predictors,
# it's easier to just separate into a training set and a cross-validation set.

open.train <- agg.open[1:3141,]
open.xval <- agg.open[3142:3926,-3]
open.true <- agg.open[3142:3926, 3]

fit.time <- proc.time()
gls_fit.huge <- gls(aapl ~ ., data=open.train, correlation = corARMA(p=1), method="ML",
                    control = list(singular.ok = TRUE))
proc.time() - fit.time

# One minute total - that could be much worse!
# Now, we just predict on our cross-validation set:

open.pred <- predict(gls_fit.huge, open.xval)
pred.res <- open.pred-open.true
summary(pred.res)

# Ouch - "-352.40" minimum and "-74.91" median aren't something we want to see!
# This is likely due to the accelerating upward nature of apple stock, though.
# What if we regress exclusively on a log scale instead?
# We'd assume that our zeroes might through some havoc into the equation...
# Let's try this a new way.

agg.open <- read.csv("agg_open.csv", header=T, row.names=1)
open.train <- log(agg.open[1:3141,])
open.xval <- log(agg.open[3142:3926,-3])
open.true <- agg.open[3142:3926, 3]

fit.time <- proc.time()
gls_fit.huge <- gls(aapl ~ ., data=open.train, correlation = corARMA(p=1), method="ML",
                    control = list(singular.ok = TRUE), na.action = na.omit)
proc.time() - fit.time

# This still throws an error; it's not really what we'd hope for here.
# Let's try, instead of replacing with 0, imputing 1? This provides us log(NA) = 0.

agg.open <- read.csv("agg_open.csv", header=T, row.names=1)
agg.open[is.na(agg.open)] <- 1
open.train <- log(agg.open[1:3141,])
open.xval <- log(agg.open[3142:3926,-3])
open.true <- agg.open[3142:3926, 3]

fit.time <- proc.time()
gls_fit.huge <- gls(aapl ~ ., data=open.train, correlation = corARMA(p=1), method="ML",
                    control = list(singular.ok = TRUE), na.action = na.omit)
proc.time() - fit.time

open.pred <- predict(gls_fit.huge, open.xval)
pred.res <- exp(open.pred)-open.true
summary(pred.res)

# This performs even worse; -491.80 minimum, -96.56 median. This is clearly not what we want!

# All of this is interesting enough; however, we shouldn't look too hard into this.
# After all, if we have a given stock's close price for the day, we should also have aapl.
# We should look towards predicting aapl based on previous stock prices only.