#Author Ranganath Srinivasan Kalaimani 
# Submitted to Monash University - ETF5231
library(readxl)
library(fpp2)
library(tidyverse)

# plotting the allocated time series data 
retail_data <- read_xlsx("RetailDataIndividual.xlsx", skip=3) %>%
  pull("29360714") %>%
  ts(start = c(1982,4), frequency = 12)
autoplot(retail_data) + xlab('Year') + ylab('Turnover in Millions') + ggtitle('Food Retail Turnover in South Australia')

## Assignment 2 ##
# plotting the seasonal plot and subsereis plot for the time series data
ggseasonplot(retail_data) + xlab('Year') + ylab('Turnover in Millions') + ggtitle('Seasonal plot Food Retail Turnover in South Australia')
ggsubseriesplot(retail_data) + xlab('Year') + ylab('Turnover in Millions') + ggtitle('SubSeries plot of Food Retail Turnover in South Australia')

# Tranforming the data and plotting 
#Box Cox tranformation
# Getting the lambda value from bocxox function for tranforming my data 
lambda <- BoxCox.lambda(retail_data)
retail_data %>% BoxCox(lambda) %>%
  autoplot() + 
  ylab(paste("BoxCox(turnover,", round(lambda, 2), ")")) + ggtitle('Boxcox Transformation')

# Log Transformation 
autoplot(BoxCox(retail_data,lambda = 0))  + xlab("Year") + ylab("Turnover in millions") + ggtitle("Natural Log Transformation")
# Inverse tranformation 
autoplot(BoxCox(retail_data,lambda = -1))  + xlab("Year") + ylab("Turnover in millions") + ggtitle("Inverse Transformation")

# splitting time series data as training and test data 
retail_1 <- window(retail_data, end = c(2014,11))
retail_2 <- window(retail_data, start = c(2015,1))

# plotting the train data and test data 
autoplot(retail_data) + autolayer(retail_1, series = 'Training') + autolayer(retail_2, series = 'Testing')

# different forecasting methods on training data 
rwf_forecast <- rwf(retail_1, h = 24)
mean_forecast <- meanf(retail_1 , h = 24)
naive_forecast <- naive(retail_1 , h = 24 )
snaive_forecast <- snaive(retail_1, h =24 )

# Checking the parameter and accuracies of forecating methods 
accuracy(mean_forecast,retail_2)
accuracy(snaive_forecast,retail_2)

# Plotting the point forecast from different forecasting methods 
autoplot(retail_1) + 
  autolayer(snaive_forecast,series = "snaive", PI = FALSE) +
  autolayer(mean_forecast, series = "mean" , PI = FALSE)

# Checking the residuals 
checkresiduals(snaive_forecast)
autoplot(snaive(retail_data, h =24))

## Assignment 3 ##
# Fitting the chosen ETS model for the time series data 
ets_fit <- ets(retail_data, model = 'MAM', damped = F)
autoplot(ets_fit) # plotting the fit
ets_fit # Checking the estimates and smoothing parameters 
checkresiduals(ets_fit) # checking the residual 

# Fittng an ETS model for the time series data 
ets_fit1 <- ets(retail_data)
autoplot(ets_fit1) # plotting the fit
ets_fit1
checkresiduals(ets_fit1) # checking the residual

# Forecating with the ETS model 
ets_fit1 %>% forecast() %>% autoplot()

# Comparing the forecast for the time sereis data
ets_fit1 %>% forecast() %>% autoplot(include = 12*8) + 
  autolayer(forecast(ets_fit)$mean)

## Assignment 4 ##

# Plotting the data with lod tranformed and with seasonally differenced
cbind('South Australia - Food Retail' = retail_data,
      'Log Transformed' = log(retail_data),
      'Seasonal Differenced' = diff(log(retail_data),lag =12)) %>% 
  autoplot(facets = T) + xlab('Year') + ylab('') +
  ggtitle('Monthly Food Retail - South Australia')

# Log transformed and checking the ACFand PACF plots
transf_data <- BoxCox(retail_data, lambda = 0)
transf_data %>% autoplot()
ggtsdisplay(transf_data)

# Seasonal differencing the log transformed data
transf_data %>% diff(lag=12) -> diff_data
nsdiffs(diff_data)
ndiffs(diff_data)
# plotting the ACF and PACF plots
diff_data %>% ggtsdisplay()

# Fitting the chosen ARIMA model fir Ts data
ARIMA_fit1 <- Arima(retail_data, order = c(3,0,0), seasonal = c(3,1,3), lambda = 0)
ARIMA_fit1 %>% checkresiduals()
# Observing the ACF and PACF plots
ARIMA_fit1 %>% residuals() %>% ggtsdisplay()

# Alternate ARIMA models for the Ts data
ARIMA_fit2 <- Arima(retail_data, order = c(3,0,0), seasonal = c(3,1,0), lambda = 0)
ARIMA_fit3 <- Arima(retail_data, order = c(3,0,0), seasonal = c(2,1,3), lambda = 0)
ARIMA_fit4 <- Arima(retail_data, order = c(3,0,0), seasonal = c(1,1,3), lambda = 0)

# Checking the model parameters 
ARIMA_fit2
ARIMA_fit3
ARIMA_fit4

# Model using auto.arima function 
ARIMA_Auto_fit1 <-  auto.arima(retail_data, lambda = 0)
ARIMA_Auto_fit1 %>% checkresiduals()
# Using auto.arima function with stepwise and approximation setting False 
ARIMA_Auto_fit2 <- auto.arima(retail_data, stepwise = F, approximation = F, lambda = 0)
ARIMA_Auto_fit2 %>% checkresiduals()

# Checking the accuracies of the models 
accuracy(ARIMA_fit1)
accuracy(ARIMA_Auto_fit1)
accuracy(ARIMA_Auto_fit2)

# forecasting with suitable chosen model after comparisons 
ARIMA_fit1 %>% forecast() %>% autoplot()

# Plotting the RetailDataIndividualFull.xlsx includes data for the period 2017-2018
retail_data_full <- read_xlsx("RetailDataIndividualFull.xlsx", skip=3) %>%
  pull("29360714") %>%
  ts(start = c(1982,4), frequency = 12)
autoplot(retail_data_full) + xlab('Year') + ylab('Turnover in Millions') + 
  ggtitle('Food Retail Turnover in South Australia') +
  autolayer(forecast(ARIMA_fit1), series = 'Forecast from ARIMA', PI = F)

# Comapring all the forecast of all models used for the ts series 
autoplot(forecast(ARIMA_fit1),xlab = 'Year', ylab = 'Retail Turnover in milliions', PI = F, alpha = 0.6) + 
  autolayer(forecast(ets_fit1), series = 'ETS', PI = F, alpha = 0.5) + 
  autolayer(snaive(retail_data), series = 'SNAIVE', PI = F, alpha = 0.3) + ggtitle('Forecast Comparison')

# comaparing the accuracies of the models 
accuracy(ARIMA_fit1)
accuracy(forecast(ets_fit1))
accuracy(snaive(retail_data))

# fitting the suitable chosen model for the RetailDataIndividualFull data 
fit_fulldata <- Arima(retail_data_full, order = c(3,0,0), seasonal = c(3,1,3), lambda = 0)
fit_fulldata %>% forecast(level = 80) %>% autoplot() + xlab('Year') + ylab('Turnover in Millions')