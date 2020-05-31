
install.packages("readxl")
library(readxl)
install.packages("forecast")
library(forecast)
library(ggplot2)
install.packages("portes")
library(portes)

setwd("C:/Users/razow/Desktop/IESEG Subjects/14 Forescasting")
getwd()

turnover <- read_excel("DataSets2020.xlsx", sheet = "Turnover")

###### Exercise 1 ######

#### 1) and 2)

turn <- ts(turnover[,2], frequency = 12, start = 2000)
turn

turn <- log(turn)

autoplot(turn)+ggtitle("Belgian industry turnover index for manufacturing of beverages")

ggseasonplot(turn, year.labels = TRUE, year.labels.left = TRUE) + ggtitle("Seasonal plot")

ggsubseriesplot(turn) + ggtitle("Seasonal subseries plot")

# Autocorrelation
ggAcf(turn)
lag.plot(turn)

ggAcf(turn, plot = FALSE)

#### 3)

# Splitting in training and test set
turn_train <- window(turn, end = 2015)
turn_test <- window(turn, start = 2016)
h <- length(turn_test)

# Applying seasonal naive forecasting and plotting
turn_sn <- snaive(turn_train, h=h)
autoplot(turn_sn)
lines(turn_test, col="red")

#Evaluating forecast accuracy
accuracy(turn_sn, turn_test)

# Getting residuals
checkresiduals(turn_sn)
res <- residuals(turn_sn)
#plot(res)
#hist(res)
tsdisplay(res)
Box.test(res, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res[-c(1:12),], lags = seq(1, 24, 1), order = 0)

# Recombine training and test set and apply the selected model to the complete data set
Turn_final <- snaive(turn, h=h)
autoplot(turn)

#### 4)

# Applying STL decomposition

turn_stl <- stl(turn_train[,1], s.window = "periodic")
plot(turn_train, col = "black", main = "Belgian Turnover Index", ylab = "Index", xlab = "")
lines(turn_stl$time.series[,2], col = "red")

autoplot(turn_stl)

# Since stl only decomposes the data, we are not getting a forecast yet. In order to be able to obtain accuracy and residuals,
# it is necessar to apply forecast to the decomposed data

# Applying naive forecast method to the stl data and evaluating forecast accuracy
turn_stl_nai <- forecast(turn_stl, method = "naive") 
autoplot(turn_stl_nai)

# Getting accuracy
accuracy(turn_stl_nai, turn_test)

# Getting residuals
checkresiduals(turn_stl_nai)
res_stl_nai <- residuals(turn_stl_nai)
res_stl_nai

tsdisplay(res_stl_nai)
Box.test(res_stl_nai, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_stl_nai[-1], lags = seq(1, 24, 1), order = 0)

#### 5)

# Applying Holt-Winters' method to forecast Turnover
turn_hw <- hw(turn_train, damped = TRUE, seasonal = "multiplicative", h=h)
autoplot(turn_hw)
summary(turn_hw)

# Evaluating forecast accuracy
accuracy(turn_hw, turn_test)

# Getting residuals
checkresiduals(turn_hw)
res_holtw <- residuals(turn_hw)

tsdisplay(res_holtw)
Box.test(res_holtw, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_holtw, lags = seq(1, 24, 1), order = length(turn_hw$model$par)) ## The residuals in this method cannot be considered as white noise
# There is some information in the residuals that we did not captured

turn_hw$model
turn_hw$model$par

#### 6)

# Applying only the methods that involve trend and seasonal components  

# ETS1
ets1 <- ets(turn_train, model = "ANA", ic = c("aic"))
ets1
ets1 <- forecast(ets1, h=h)
autoplot(ets1)

# Evaluating forecast accuracy
accuracy(ets1, turn_test)

# Getting residuals
checkresiduals(ets1)
res_ets1 <- residuals(ets1)

tsdisplay(res_ets1)
Box.test(res_ets1, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets1, lags = seq(1, 24, 1), order = length(ets1$model$par))

# ETS2
ets2 <- ets(turn_train, model = "ANAd", ic = c("aic"))
ets2
ets2 <- forecast(ets2, h=h)
autoplot(ets2)

# Evaluating forecast accuracy
accuracy(ets2, turn_test)

# Getting residuals
checkresiduals(ets2)
res_ets2 <- residuals(ets2)

tsdisplay(res_ets2)
Box.test(res_ets2, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets2, lags = seq(1, 24, 1), order = length(ets2$model$par))

# ETS3
ets3 <- ets(turn_train, model = "AAA", ic = c("aic"))
ets3
ets3 <- forecast(ets3, h=h)
autoplot(ets3)

# Evaluating forecast accuracy
accuracy(ets3, turn_test)

# Getting residuals
checkresiduals(ets3)
res_ets3 <- residuals(ets3)

tsdisplay(res_ets3)
Box.test(res_ets3, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets3, lags = seq(1, 24, 1), order = length(ets3$model$par))

# ETS4
ets4 <- ets(turn_train, model = "AAAd", ic = c("aic"))
ets4
ets4 <- forecast(ets4, h=h)
autoplot(ets4)

# Evaluating forecast accuracy
accuracy(ets4, turn_test)

# Getting residuals
checkresiduals(ets4)
res_ets4 <- residuals(ets4)

tsdisplay(res_ets4)
Box.test(res_ets4, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets4, lags = seq(1, 24, 1), order = length(ets4$model$par))

# ETS5
ets5 <- ets(turn_train, model = "MNA", ic = c("aic"))
ets5
ets5 <- forecast(ets5, h=h)
autoplot(ets5)

# Evaluating forecast accuracy
accuracy(ets5, turn_test)

# Getting residuals
checkresiduals(ets5)
res_ets5 <- residuals(ets5)

tsdisplay(res_ets5)
Box.test(res_ets5, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets5, lags = seq(1, 24, 1), order = length(ets5$model$par))

# ETS6
ets6 <- ets(turn_train, model = "MNAd", ic = c("aic"))
ets6
ets6 <- forecast(ets6, h=h)
autoplot(ets6)

# Evaluating forecast accuracy
accuracy(ets6, turn_test)

# Getting residuals
checkresiduals(ets6)
res_ets6 <- residuals(ets6)

tsdisplay(res_ets6)
Box.test(res_ets6, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets6, lags = seq(1, 24, 1), order = length(ets6$model$par))

# ETS7
ets7 <- ets(turn_train, model = "MAA", ic = c("aic"))
ets7
ets7 <- forecast(ets7, h=h)
autoplot(ets7)

# Evaluating forecast accuracy
accuracy(ets7, turn_test)

# Getting residuals
checkresiduals(ets7)
res_ets7 <- residuals(ets7)

tsdisplay(res_ets7)
Box.test(res_ets7, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets7, lags = seq(1, 24, 1), order = length(ets7$model$par))

# ETS8
ets8 <- ets(turn_train, model = "MAAd", ic = c("aic"))
ets8
ets8 <- forecast(ets8, h=h)
autoplot(ets8)

# Evaluating forecast accuracy
accuracy(ets8, turn_test)

# Getting residuals
checkresiduals(ets8)
res_ets8 <- residuals(ets8)

tsdisplay(res_ets8)
Box.test(res_ets8, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_ets8, lags = seq(1, 24, 1), order = length(ets8$model$par))

# Applying auto ETS to get the best model automatically 
turn_ets <- ets(turn_train, ic = c("aic"))
turn_ets
autoplot(turn_ets)

# Applying forecast to the selected ETS model
turn_etsf <- forecast(turn_ets, h=h)
autoplot(turn_etsf)

# Evaluating forecast accuracy
accuracy(turn_etsf, turn_test)

# Getting residuals
checkresiduals(turn_etsf)
res_etsf <- residuals(turn_etsf)

tsdisplay(res_etsf)
Box.test(res_etsf, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_etsf, lags = seq(1, 24, 1), order = length(turn_etsf$model$par))

#### 7)

# Applying differences to the data and identification of autocorrelations
tsdisplay(turn_train, main = "Belgian Turnover Index")
tsdisplay(diff(turn_train, 12), main = "Seasonality differenced Belgian turnover Index")

# Suggesting ARIMA models manually
fit1 <- Arima(turn_train, order = c(1,0,1), seasonal = c(2,1,1))
tsdisplay(residuals(fit1))

fit2 <- Arima(turn_train, order = c(3,0,1), seasonal = c(0,1,1))
tsdisplay(residuals(fit2))

fit3 <- Arima(turn_train, order = c(3,0,1), seasonal = c(3,1,1))
tsdisplay(residuals(fit3))

fit4 <- Arima(turn_train, order = c(3,1,1), seasonal = c(3,1,1))
tsdisplay(residuals(fit4))

fit5 <- Arima(turn_train, order = c(3,0,3), seasonal = c(3,1,1))
tsdisplay(residuals(fit5))

fit6 <- Arima(turn_train, order = c(3,0,3), seasonal = c(3,1,3)) # Best results at least in terms of white noise 
tsdisplay(residuals(fit6))

# Applying auto ARIMA
fit7 <- auto.arima(turn_train, allowdrift = FALSE, stepwise = FALSE, approximation = FALSE)
fit7
fc_autoARIMA <- forecast(fit7, h=h)
autoplot(fc_autoARIMA)

# Getting accuracy
accuracy(fc_autoARIMA, turn_test)

# Getting residuals and LjungBox test
checkresiduals(fit7)
res_auto <- residuals(fc_autoARIMA)

tsdisplay(res_auto)
Box.test(res_auto, lag = 24, fitdf = 0, type = "Lj")
LjungBox(fit7$residuals, lags=seq(1,24,1), order=length(fit7$coef))

### 8)
# Please refer to the report to find the conslusion about the previous steps.


### 9)

# Generating forecast over the complete data and using the final model
Final_Forcast <- ets(turn, model = "AAA")
Final_Forcast
autoplot(Final_Forcast)

# Applying forecast to the selected ETS model
Final_Forcastf <- forecast(Final_Forcast, h=h)
autoplot(Final_Forcastf)

# Evaluating forecast accuracy
accuracy(Final_Forcastf)

# Getting residuals
checkresiduals(Final_Forcastf)
res_Final_Forcastf <- residuals(Final_Forcastf)

tsdisplay(res_Final_Forcastf)
Box.test(res_Final_Forcastf, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_Final_Forcastf, lags = seq(1, 24, 1), order = length(Final_Forcastf$model$par))

### 10)

# Applying STL and Seasonal Adjustment to the complete data
fit_stl <- stl(turn[,1], s.window = "periodic", robust = TRUE)
turn_adj <- seasadj(fit_stl)
tsdisplay(turn_adj, main = "Seasonal Adjusted Belgian Turnover Index")

# Applying AUTO ARIMA to the adjusted data
auto_arima_turn <- auto.arima(turn_adj,allowdrift = FALSE, stepwise = FALSE, approximation = FALSE)
auto_arima_turn

# Applying forecast to the selected ARIMA model
fc_auto_arima_turn <- forecast(auto_arima_turn, h=h)
autoplot(fc_auto_arima_turn)

# Getting accuracy
accuracy(fc_auto_arima_turn)

# Getting residuals and LjungBox test
checkresiduals(auto_arima_turn)
res_adj_auto <- residuals(fc_auto_arima_turn)

tsdisplay(res_adj_auto)
Box.test(res_adj_auto, lag = 24, fitdf = 0, type = "Lj")
LjungBox(auto_arima_turn$residuals, lags=seq(1,24,1), order=length(auto_arima_turn$coef))

##############################

# Applying AUTO ETS to the adjusted data
auto_ets_turn <- ets(turn_adj)
auto_ets_turn
autoplot(auto_ets_turn)

# Applying forecast to the selected ETS model
auto_ets_turn_fc <- forecast(auto_ets_turn, h=h)
autoplot(auto_ets_turn_fc)

# Evaluating forecast accuracy
accuracy(auto_ets_turn_fc)

# Getting residuals
checkresiduals(auto_ets_turn_fc)
res_auto_etsfc <- residuals(auto_ets_turn_fc)

tsdisplay(res_auto_etsfc)
Box.test(res_auto_etsfc, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res_auto_etsfc, lags = seq(1, 24, 1), order = length(auto_ets_turn_fc$model$par))

###### Exercise 2 ######

electricity <- read_excel("electricity.xlsx")

electricity <- ts(electricity[,3], frequency = 12, start = 1990)
electricity

tsdisplay(electricity)

electricity <- log(electricity)

autoplot(electricity)+ggtitle("Residential Electricity Revenue - New York")

ggseasonplot(electricity, year.labels = TRUE, year.labels.left = TRUE) + ggtitle("Seasonal plot")

ggsubseriesplot(electricity) + ggtitle("Seasonal subseries plot")

# Autocorrelation
ggAcf(electricity)
lag.plot(electricity)

ggAcf(electricity, plot = FALSE)

# Splitting in training and test set
electricity_train <- window(electricity, end = c(2012, 12))
electricity_test <- window(electricity, start = c(2013,1))
h <- length(electricity_test)

### Seasonal Naive ###

# Applying seasonal naive forecasting and plotting
electricity_sn <- snaive(electricity_train, h=h)
autoplot(electricity_sn) +
lines(electricity_test, col="red")

#Evaluating forecast accuracy
accuracy(electricity_sn, electricity_test)

# Getting residuals
checkresiduals(electricity_sn)
res <- residuals(electricity_sn)

tsdisplay(res)
Box.test(res, lag = 24, fitdf = 0, type = "Lj")
LjungBox(res[-c(1:12),], lags = seq(1, 24, 1), order = length(electricity_sn$coef))

### Seasonal auto ARIMA ###

# Applying auto ARIMA
elec_arima <- auto.arima(electricity_train, allowdrift = FALSE, stepwise = FALSE, approximation = FALSE)
elec_arima
fc_elec_arima <- forecast(elec_arima, h=h)
autoplot(fc_elec_arima)

# Getting accuracy
accuracy(fc_elec_arima, electricity_test)

# Getting residuals and LjungBox test
checkresiduals(fc_elec_arima)
res_elec_arima <- residuals(fc_elec_arima)

tsdisplay(res_elec_arima)
Box.test(res_elec_arima, lag = 24, fitdf = 0, type = "Lj")
LjungBox(fc_elec_arima$residuals, lags=seq(1,24,1), order=length(fc_elec_arima$coef))

# Applying ARIMA model on the whole data

electricity_fc <- Arima(electricity, order = c(4,0,0), seasonal = c(0,1,1))
electricity_fc <- forecast(electricity_fc, h=h)
autoplot(electricity_fc)


