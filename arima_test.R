# FILE   : arima_test.R
# PURPOSE: Using our stock data, perform time series analysis.
# NOTES  : Writen by Yijun Yang.
#          (Only uploaded by Ethan Edwards because of some Git issues.)

View(stocks$aapl)
aapl.ts <- ts(stocks$aapl$Close)
plot(aapl.ts)

acf(log(aapl.ts))
acf(aapl.ts)

plot(diff(log(aapl.ts)))
acf(diff(log(aapl.ts)))

get.best.arima <- function(aapl.ts, maxord = c(1,1,1,1,1,1)){best.aic <- 1e8
                                                             n <- length(aapl.ts)
                                                             for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
                                                               for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
                                                               {
                                                                 fit <- arima(aapl.ts, order = c(p,d,q),
                                                                              seas = list(order = c(P,D,Q),
                                                                                          frequency(aapl.ts)), method = "CSS")
                                                                 fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
                                                                 if (fit.aic < best.aic)
                                                                 {best.aic <- fit.aic
                                                                  best.fit <- fit
                                                                  best.model <- c(p,d,q,P,D,Q)
                                                                 }
                                                               }
                                                             list(best.aic, best.fit, best.model)
}

best.arima.im<- get.best.arima((log(aapl.ts)), maxord = c(2,2,2,2,2,2))

best.fit.im <- best.arima.im [[2]]
acf( resid(best.fit.im))
pacf( resid(best.fit.im))
best.arima.im

ts.plot( cbind( window(aapl.ts,start = 1985),exp(predict(best.fit.im,10)$pred) ), lty = 1:3)

exp(predict(best.fit.im,10)$pred)



model1= arima(log(aapl.ts), order = c(0,0,0),seas  =list(order = c(2,1,0),12))

AIC(model1)
# layout(c(1,1,2,2))
acf(model1$resid)
pacf(model1$resid)
plot(model1)
