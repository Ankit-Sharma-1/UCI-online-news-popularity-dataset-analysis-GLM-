setwd("C:/Ankit/Y2S1/DATA7202 Statistical Methods for Data Science/Assignment/Assignment_2")

# flowdata1 = read.csv(file = 'Data/Flow_data/flow_20130225-20130303.csv',header =
#                        TRUE,skip = 1)
# flowdata2 = read.csv(file = 'Data/Flow_data/flow_20130304-20130310.csv',header =
#                        TRUE,skip = 1)
# flowdata3 = read.csv(file = 'Data/Flow_data/flow_20130311-20130317.csv',header =
#                        TRUE,skip = 1)
# flowdata4 = read.csv(file = 'Data/Flow_data/flow_20130318-20130324.csv',header =
#                        TRUE,skip = 1)


#install.packages("tseries")
library(tseries)


#Combine datasets
#flow_combined = rbind(flowdata1,flowdata2,flowdata3,flowdata4)

#Filter for traffic from region 5 to region 1
# flow_filtered = flow_combined[which(flow_combined$region_from== 5),]
# flow_filtered = flow_filtered[which(flow_filtered$region_to == 1),]

flow_filtered = read.csv(file = 'Data/Flow_data/flow_processed.csv',header = TRUE)

flow_filtered$day = weekdays(as.Date(flow_filtered$date))

flow_filtered$date = as.Date(flow_filtered$date)


ts_flow = ts(flow_filtered$v0_num_traj,frequency = 143)
summary(ts_flow)

png(filename=paste("./Plot2/1.1_Time Series Plot",".png"), width = 1280, height = 480)
plot(ts_flow)
dev.off()

#cycle(): gives the positions in the cycle of each observation (stats)
cycle(ts_flow)
png(filename=paste("./Plot2/1.2_All_day_Plot_cycle",".png"), width = 1280, height = 480)
boxplot(ts_flow~cycle(ts_flow))
dev.off()

#deltat(): returns the time interval between observations (stats)
deltat(ts_flow)


# Averages model 

flow_hourlymean = c( 23.95,
                     126.8,
                     281.1,
                     281.65,
                     152.15,
                     102.85,
                     94.7,
                     85.2,
                     74.55,
                     63.7,
                     50.5,
                     46.45,
                     73.8,
                     75.65,
                     41.05,
                     30.65,
                     23.9,
                     20.85,
                     8.55,
                     14.625,
                     2.375,
                     0.625,
                     0.375,
                     0.75,
                     1.25,
                     29.625,
                     57.5,
                     130.625,
                     185.5,
                     191.75,
                     195.75,
                     198.625,
                     179.5,
                     145.375,
                     116.25,
                     113.75,
                     151.75,
                     122,
                     91.875,
                     60.875,
                     58.25,
                     59.125,
                     28.25)
                     

png(filename=paste("./Plot2/1.3 Averages Model",".png"), width = 1280, height = 480)
ts.plot(c(flow_filtered$v0_num_traj,flow_hourlymean), ylab="Passengers travelling")
dev.off()

# Test for stationarity
# Tests the null hypothesis that a unit root is present in a time series sample. 
# The alternative hypothesis is different depending on which version of the test is used, but is usually stationarity or trend-stationarity

#install.packages("urca")
library("urca")

#Check if stationary
adf.test(diff(ts_flow), alternative="stationary", k=0)

#Dickey-Fuller = -16.65, Lag order = 0, p-value = 0.01
#alternative hypothesis: stationary

#Autocorrelation check 
png(filename=paste("./Plot2/1.4_ACF",".png"), width = 640, height = 480)
acf(ts_flow, main='ACF for Differenced Series')
dev.off()

#Partial correlation check 
png(filename=paste("./Plot2/1.5_PACF",".png"), width = 640, height = 480)
pacf(ts_flow, main='PACF for Differenced Series')
dev.off()


#model 2
#install.packages("forecast")
library(forecast)

#model 2.1, ARMA model with order (p=5, d = 0, q=4)
ARMA_fit_2.1 = arima(ts_flow,order = c(5,0,4))
forecast(ARMA_fit_2.1, 143)
png(filename=paste("./Plot2/2.1_ARMA",".png"), width = 640, height = 480)
plot(forecast(ARMA_fit_2.1, 143))
dev.off()

#model 2.2, ARMA model with order (p=5, d = 1, q=4)
ARMA_fit_2.2 = arima(ts_flow,order = c(5,1,4))
forecast(ARMA_fit_2.2, 143)
png(filename=paste("./Plot2/2.2_ARMA",".png"), width = 640, height = 480)
plot(forecast(ARMA_fit_2.2, 143))
dev.off()

#model 2.3, ARMA model with order (p=5, d = 1, q=0)
ARMA_fit_2.3 = arima(ts_flow,order = c(5,1,0))
forecast(ARMA_fit_2.3, 143)
png(filename=paste("./Plot2/2.3_ARMA",".png"), width = 640, height = 480)
plot(forecast(ARMA_fit_2.3, 143))
dev.off()

#model 2.4, ARMA model with order (p=0, d = 1, q=4)
ARMA_fit_2.4 = arima(ts_flow,order = c(0,1,4))
forecast(ARMA_fit_2.4, 143)
png(filename=paste("./Plot2/2.4_ARMA",".png"), width = 640, height = 480)
plot(forecast(ARMA_fit_2.4, 143))
dev.off()

#model 2.5, Seasonality 
ts_flow.decomp= decompose(ts_flow)
png(filename=paste("./Plot2/2.5 ARMA_seasonal",".png"), width = 640, height = 480)
plot(ts_flow.decomp$seasonal,  main='Seasonal Component of Model')
dev.off()


#model 2.6, ARMA model with order (p=5, d=0, q=4) and seasonal (p=0, d=1, q=0)

ARMA_fit_2.6 = arima(ts_flow,order = c(5,0,4), seasonal = list(order = c(0, 1, 0),period = 143))
forecast(ARMA_fit_2.6, 143)
png(filename=paste("./Plot2/2.6_ARMA",".png"), width = 640, height = 480)
plot(forecast(ARMA_fit_2.6, 143))
dev.off()
 
#model 2.7, ARMA model with order (p=0, d=1, q=4) and seasonal (p=0, d=1, q=1) 

ARMA_fit_2.7 = arima(ts_flow,order = c(5,0,4), seasonal = list(order = c(0, 1, 1),period = 143))
forecast(ARMA_fit_2.7, 143)
png(filename=paste("./Plot2/2.7_ARMA",".png"), width = 640, height = 480)
plot(forecast(ARMA_fit_2.7, 143))
dev.off()

autoArimaFit <- auto.arima(ts_flow, seasonal=TRUE)
#forecast(autoArimaFit,143)
# png(filename=paste("./Plot2/2.8 ARIMA",".png"), width = 640, height = 480)
# plot(forecast(autoArimaFit, 143))
# dev.off()

#Prediction one hour ahead

flow_one_hour_ahead_prediction_data = read.csv(file = 'Data/Flow_data/flow_one_hour_ahead_prediction.csv',header = TRUE)

#flow_one_hour_ahead_prediction_data$day = weekdays(as.Date(flow_one_hour_ahead_prediction_data$date))

#flow_one_hour_ahead_prediction_data$date = as.Date(flow_one_hour_ahead_prediction_data$date)

ts_flow_one_hour_ahead = ts(flow_one_hour_ahead_prediction_data$v0_num_traj,frequency = 143)

ARMA_fit_one_hour_ahead = arima(ts_flow_one_hour_ahead,order = c(5,0,4), seasonal = list(order = c(0, 1, 0),period = 143))
forecast(ARMA_fit_one_hour_ahead, 143)


#Prediction two hour ahead

flow_two_hour_ahead_prediction_data = read.csv(file = 'Data/Flow_data/flow_two_hour_ahead_prediction.csv',header = TRUE)

#flow_two_hour_ahead_prediction_data$day = weekdays(as.Date(flow_two_hour_ahead_prediction_data$date))

#flow_two_hour_ahead_prediction_data$date = as.Date(flow_two_hour_ahead_prediction_data$date)

ts_flow_two_hour_ahead = ts(flow_two_hour_ahead_prediction_data$v0_num_traj,frequency = 143)

ARMA_fit_two_hour_ahead = arima(ts_flow_two_hour_ahead,order = c(5,0,4), seasonal = list(order = c(0, 1, 0),period = 143))

forecast(ARMA_fit_two_hour_ahead, 143)


#Prediction three hour ahead

flow_three_hour_ahead_prediction_data = read.csv(file = 'Data/Flow_data/flow_three_hour_ahead_prediction.csv',header = TRUE)

#flow_three_hour_ahead_prediction_data$day = weekdays(as.Date(flow_three_hour_ahead_prediction_data$date))

#flow_three_hour_ahead_prediction_data$date = as.Date(flow_three_hour_ahead_prediction_data$date)

ts_flow_three_hour_ahead = ts(flow_three_hour_ahead_prediction_data$v0_num_traj,frequency = 143)

ARMA_fit_three_hour_ahead = arima(ts_flow_three_hour_ahead,order = c(5,0,4), seasonal = list(order = c(0, 1, 0),period = 143))

forecast(ARMA_fit_three_hour_ahead, 143)


