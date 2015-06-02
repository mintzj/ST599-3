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
pacf(aapl.ts)

# PACF spike only at term 1, ACF spikes descending slowly from 1.
# This is almost certainly an AR(1) series of difference zero.

AIC(arima(aapl.ts, order=c(1,0,0), method="ML"),
    arima(aapl.ts, order=c(0,0,1), method="ML"),
    arima(aapl.ts, order=c(1,0,1), method="ML"))

acf(resid(arima(aapl.ts, order=c(1,0,0), method="ML", seasonal=c(1,0,0))))

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

gls_fit.100 <- gls(aapl.lg ~ goog.lg, correlation = corARMA(p=1), method="ML")
acf(residuals(gls_fit.100, type="normalized"))
pacf(residuals(gls_fit.100, type="normalized"))

# Interesting; PACF possibly hints at mild seasonality every 12? Look into this!

qqnorm(residuals(gls_fit.100, type="normalized"))
# Not ideal, but at last distributed decently-ish.
qplot(y=residuals(gls_fit.100, type="normalized"), x=1:2260, geom="line")
# also not idea, but, well.

round(intervals(gls_fit.100)$coef, 2)
round(intervals(gls_fit.100)$corStruct, 2)
# Throwing errors?