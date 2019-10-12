##### Codes file for Assignment of Time Series Forecasting #####

## Loading required packages
suppressMessages(library(ggplot2))
suppressMessages(library(tseries))
suppressMessages(library(forecast))

## Loading dataset to the R environment
data <- read.csv("fancy.txt", header = F)
str(data)
class(data)

## Converting the dataset into a "ts" class
fancy_ts <- ts(data, start = c(1987, 1), end = c(1993, 12), 
                  frequency = 12)
class(fancy_ts)

## Let's see the start, end and frequency of the dataset
# Start of the dataset
start(fancy_ts)

# End of the dataset
end(fancy_ts)

# Frequency of the dataset
frequency(fancy_ts)

## Visualizing the dataset
autoplot(fancy_ts, main = "Souvenir Shop Monthly Sales", ylab = " ", xlab = "Year")

## Decomposing the dataset
plot(decompose(fancy_ts))


## Time Series Modeling

## Our task is to build a model to forecast sales for the next 5 years

## Part-A: Building a forecasting model using Holt's Winter Method
## Holt's Winter Seasonal Model
hw.model <- HoltWinters(fancy_ts)
hw.model

## Plotting the model's forecast for previous periods over original series
plot(hw.model)

## Stability of the model
# mean of model residuals
mean(hw.model.f$residuals, na.rm = T)

# normality of residuals
shapiro.test(hw.model.f$residuals)

# autocorrelation of residuals
Box.test(hw.model.f$residuals, type = "Ljung-Box")

# checking the stability of the model visually
checkresiduals(hw.model)

## Accuracy of the model
accuracy(hw.model.f)

## Forecasting for the next 5 years
hw.model.f <- forecast(hw.model, 60)
hw.model.f

## Plotting the forecasted values along with the original series
autoplot(hw.model.f)


## Part-B: Building a forecasting model using ARIMA Method
## ARIMA Model

## Checking the stationarity of dataset

# kpss test of stationarity
kpss.test(fancy_ts)

# adf test os stationarity
adf.test(fancy_ts)

## Making the series stationary by differencing and transformation
#  Taking care of the variance first
autoplot(log(fancy_ts))

# Taking care of the mean now
autoplot(diff(log(fancy_ts)))

## Checking the stationarity of the transformed series

# kpss test of stationarity 
kpss.test(diff(log(fancy_ts)))

# adf test of stationarity
adf.test(diff(log(fancy_ts)))

# the p value from both the test, for the converted series, comes out to 
# be significant, indicating that now the series is stationary

## Developing ARIMA (p, d, q) Model 
# ARIMA has three components - AR, I & MA component
# AR is the Auto Regressive component, I is the order of integration & MA is 
# the moving average component

# PACF plot to get the value of p
pacf(diff(log(fancy_ts)))

## ACF plot to get the value of q
acf(diff(log(fancy_ts)))

## Building an ARIMA model with auto.arima
arima.model1 <- auto.arima(log(fancy_ts))
arima.model1

## Building the ARIMA model of order (2, 1, 2)
arima.model2 <- arima(log(fancy_ts), c(2,1,2),
                     seasonal = list(order = c(2,1,2), period = 12))
arima.model2

## arima.model1 gives the minimum AIC value. So, keeping that as final model.

# Plotting the forecasted values along with the original series
plot(fancy_ts)
lines(2.718^arima.model1$fitted, col = "blue")

## Checking the stability of the model
# Checking the mean of residuals
mean(arima.model1$residuals)

# Checking normality of the residuals
shapiro.test(arima.model1$residuals)

# Ljung-Box test to test correlation between residuals
Box.test(arima.model1$residuals, type = "Ljung-Box")

## Residual Diagnostic Plot
checkresiduals(arima.model1)

## Accuracy of the Model
accuracy(arima.model1)

## Forecasting for the next 5 years
arima.f <- forecast(arima.model1, h = 60)

# Plotting the forecasted values
autoplot(arima.f)

# Getting the final values by taking antilog
fancy.f <- 2.718^arima.f$mean
fancy.f


