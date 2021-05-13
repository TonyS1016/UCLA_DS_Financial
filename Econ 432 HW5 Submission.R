#Econ 432 HW5 R Codes Submission 

#1. 

#a.
#Using the R function arima.sim(), simulate and plot 250 observations of the 
#MA(1), theoretical ACF, and sample ACF with theta= 0.5,0.9,-0.9
ls <- list(order = c(0, 0, 1), ma = 0.5)
mu <- 1
MA1_0.5 <- arima.sim(n = 1000, model = ls, sd = 3) + mu
plot(MA1_0.5)
acf(MA1_0.5)

ls <- list(order = c(0, 0, 1), ma = 0.9)
MA1_0.9 <- arima.sim(n = 1000, model = ls, sd = 3) + mu
plot(MA1_0.9)
acf(MA1_0.9)

ls <- list(order = c(0, 0, 1), ma = -0.9)
MA1_0.9b <- arima.sim(n = 1000, model = ls, sd = 3) + mu
plot(MA1_0.9b)
acf(MA1_0.9b)

#b.
#Briefly comment on the behavior of the simulated data series 

#They all share the same general trend of having a significant spike in the 1st lag
#And then they all share oscillating positive and negative insignificant spikes for higher lags 
#There are some outliers with some slightly significant spikes but those are negligible


#2

#a. 
#Using the R function arima.sim(), 
#simulate and plot 250 observations of the AR(1) 
#with phi = 0; phi = 0:5;phi = 0.9, and phi = 0.99
set.seed(123)


lsl = list(order = c(0,0,0))
mu = 1 
AR1_0 = arima.sim(n=250, model = lsl, sd = 1) + mu
plot(AR1_0)


lsl = list(order = c(1,0,0), ar = 0.5)
mu = 1 
AR1_0.5 = arima.sim(n=250, model = lsl, sd = 1) + mu
plot(AR1_0.5)

lsl = list(order = c(1,0,0), ar = 0.9)
AR1_0.9 = arima.sim(n=250, model = lsl, sd = 1) + mu
plot(AR1_0.9)


lsl = list(order = c(1,0,0), ar = 0.99)
AR1_0.99 = arima.sim(n=250, model = lsl, sd = 1) + mu
plot(AR1_0.99)


#b.
#Comment on the behavior of the simulated data series,
#Which series is close to nonstationary or persistent time series

#As the phi gets closer to 1, the series will become more nonstationary
#And exhibit a persistent time series qualities 


#3.

