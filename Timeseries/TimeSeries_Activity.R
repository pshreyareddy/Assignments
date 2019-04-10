rm(list=ls(all=TRUE))

#1. Read the data ====

#Set the working directory:
setwd("C:/Users/Classroom 4/Desktop/20180722_Batch46_CSE7302c_TimeSeries_Lab")

#Read the data:
df = readRDS("ecommerceData.RData")

#Data exploration:
dim(df) #check number of rows and columns
names(df) #check the column names
head(df) #look at the sample data
summary(df) #summary stats of each column
length(unique(df$TitleKey)) #number of unique title keys

#2. Subset the required title key ====

#As the product prices vary differently for each product,
#let us focus at one product for now with title key: "4302628" and condition:"Good"

df2 = df[df$TitleKey == "4302628" & df$Condition == "Good",]

#Data exploration:
dim(df2) #check number of rows and columns
names(df2) #check the column names
head(df2) #look at the sample data
summary(df2) #summary stats of each column

#3. Data preprocessing / preparation ====

#Convert the data type of date appropriately
class(df2$Date)
df2$Date = as.Date(df2$Date,format = "%Y-%m-%d")
class(df2$Date)

#Arrange the data in ascending order of date
df2 = df2[order(df2$Date,decreasing = F),]
head(df2) #look at the sample data

#There are multiple records on each day,
#let us aggregate the data so that we have one record per day,
#and mean price for the day

library(dplyr)
df3 = df2 %>% group_by(Date) %>% summarise("Mean_Price" = mean(Price))
head(df3)

#since the output is a tibble convert the same to data frame
df3 = data.frame(df3)
head(df3)

#check for continuity in dates
df3["seq_check"] = df3$Date  - lag(df3$Date)
unique(df3$seq_check)

#generate continuous sequence of req dates
req_date_seq = seq(min(df3$Date),max(df3$Date),by="days")
df4 = data.frame(req_date_seq)
nrow(df4) - nrow(df3) #number of dates for which the prices are not available

#merge the two data frames
df4 = merge(df4,df3,by.x = "req_date_seq",by.y = "Date",all.x = T)
df4$seq_check = NULL

#handle the missing values
sum(is.na(df4)) #number of missing values in the data frame

library(imputeTS)
#check in help for na.locf()
df4$Mean_Price = na.locf(df4$Mean_Price,option = "locf")
sum(is.na(df4)) #number of missing values in the data frame

#4. Prepare weekly data, split train & test ====

#get the year and week information from date
df4["year"] = as.numeric(format(df4$req_date_seq,format = "%Y"))
df4["week_num"] = as.numeric(format(df4$req_date_seq,format = "%W"))

df5 = df4 %>% group_by(year,week_num) %>% summarise("Mean_Price" = mean(Mean_Price))
df5 = data.frame(df5)

dim(df5) #check number of rows and columns
names(df5) #check the column names
head(df5) #look at the sample data
summary(df5) #summary stats of each column

#since it is sequential data, random split is not possible
n = nrow(df5)
train = df5[1:(n-4),]
test = df5[(n-3):n,]

#5. Converting into time series object ====

train_price = ts(train$Mean_Price,frequency = 52)

#plot the time series
plot(train_price,type="l",lwd=3,col="red",
     xlab="week",ylab="Price",
     main="Mean weekly prices for product 'xyz'")

#Decompose the data to get Trendm, Seasonality & Randomness
train_price_decomposed = decompose(train_price)
plot(train_price_decomposed,col="Red")

#6. Model building (SMA, WMA, EMA) ====

#Modelling the time series behaviour by simple moving averages
library(TTR)
fitsma = SMA(train_price,n=2)
length(fitsma)
length(train_price)

#Define the metric MAPE 
#Train
MAPE_SMA = mean(abs((train_price[2:length(train_price)]-fitsma[2:length(train_price)])
                    /train_price[2:length(train_price)]))
MAPE_SMA

#Test
library(DMwR)
MAPE_SMA = regr.eval(trues = test$Mean_Price,fitsma[length(fitsma)])
MAPE_SMA = MAPE_SMA[4]
MAPE_SMA

#Weighted Moving Averages
fitwma= WMA(train_price,n=2,1:2)

#Define the metric MAPE 
#Train
MAPE_WMA =  regr.eval(train_price[2:length(train_price)],fitwma[2:length(train_price)])
MAPE_WMA = MAPE_WMA[4]
MAPE_WMA

#Test
MAPE_WMA = regr.eval(trues = test$Mean_Price,fitwma[length(fitwma)])
MAPE_WMA = MAPE_WMA[4]
MAPE_WMA

#Exponential Moving Averages
fitEma = EMA(train_price, n = 2)

#Define the metric MAPE 
#Train
MAPE_EMA = regr.eval(train_price[2:length(train_price)],fitEma[2:length(train_price)])
MAPE_EMA = MAPE_EMA[4]
MAPE_EMA

#Test
MAPE_EMA = regr.eval(trues = test$Mean_Price,fitEma[length(fitEma)])
MAPE_EMA = MAPE_EMA[4]
MAPE_EMA

plot(train$Mean_Price, type="l", col="black",main = "Price vs Moving averages")
lines(fitsma, col="red", lwd=2)
lines(fitwma, col="blue")
lines(fitEma, col="green")

#7. Model building (HoltWinters) ====

## Build a HoltWinters model  with trend 
holtpriceforecast = HoltWinters(train_price,gamma=FALSE)
head(holtpriceforecast$fitted)

## HoltWinters model  with trend  and Seasonality
priceholtforecast = HoltWinters(train_price, beta=TRUE, gamma=TRUE, seasonal="additive")
head(priceholtforecast$fitted)

# Since you are building the models on weekly data, you will get 52 seasonal components. 
# If you are reading the monthly data, you will get 12 seasonal components

holtforecastTrain = data.frame(priceholtforecast$fitted)
holtforecastTrainpredictions = holtforecastTrain$xhat
head(holtforecastTrainpredictions)

### Prediction on test data
library(forecast)
holtpriceforecast=  forecast(priceholtforecast,h = 4)
plot(holtpriceforecast,ylim = c(-50,200),shadecols = "oldstyle")

### Define the metric hw 
MAPE_HW = regr.eval(test$Mean_Price,holtpriceforecast$mean)
MAPE_HW = MAPE_HW[4]

#8. Stationarizing the data ====

#ACF & PACF plots
par(mfrow=c(2,2))
acf(train_price,lag=30)
pacf(train_price,lag=30)

# Autocorrelation is the linear dependence of a variable with itself at two points in time
# For stationary processes, autocorrelation between any two observations only depends on the time lag h between them
# Partial autocorrelation is the autocorrelation between yt and yhat after removing any linear dependence on y1,y2, ..., yt?h+1

# Looking at the acf anf pacf plots for 30 lags
acf(train$Mean_Price,lag=30)
pacf(train$Mean_Price,lag=30)

# Looking at the Y scale in ACF we observe that trend is more dominant than seasonality
# Data is not stationary and we need to stationarize the data
train_price1 = train_price
### Stationarize by differencing
par(mfrow=c(2,3))
plot(diff(train_price1,lag = 1),type="l"); acf(diff(train_price1,lag = 1),lag=30) ;pacf(diff(train_price1,lag = 1),lag=30)
plot(diff(train_price1,lag=2),type="l");  acf(diff(train_price1,lag = 2),lag=30); pacf(diff(train_price1,lag = 2),lag=30)

# one lag has stationarized the data; we can use ndiffs of forecast package to check no of differences required to stationarize the data
ndiffs(train_price)

#9. Model building (ARIMA) ====

model1 = arima(train_price,c(0,0,0))
model1
par(mfrow=c(3,3))
plot(train_price)
acf(train_price) 
pacf(train_price)

## Considering the difference from the graph as d=1 to stationarize
model2 = arima(train_price,c(0,1,0))
model2
plot(diff(train_price))
acf(diff(train_price,lag = 1))
pacf(diff(train_price,lag = 1))

# plot has still non stationary behaviour another difference can stationarize it 
model3 = arima(train_price,c(0,2,0))
model3
plot(diff(train_price,differences = 2))
acf(diff(train_price,differences = 2))
pacf(diff(train_price,differences = 2))

# Observing the acf and pacf there is significant lag in acf and also in pacf that has to be taken care 
model4 = arima(train_price,c(1,1,1))
model4

## Plots of the models
par(mfrow=c(2,2))
plot(model1$residuals)
plot(model2$residuals)
plot(model3$residuals)
plot(model4$residuals)

### Forecast on the models 
pricearimaforecasts1 = forecast(model1, h=4)
plot(pricearimaforecasts1)
pricearimaforecasts2 = forecast(model2, h=4)
plot(pricearimaforecasts2)
pricearimaforecast3 = forecast(model3, h=4)
plot(pricearimaforecast3)
pricearimaforecast4 = forecast(model4, h=4)
plot(pricearimaforecast4)

# Model 1 was constructed with no trend and no seasonality and therefore the prediction will be same as present.
# Model4 has both trend and seasonality.

### Define the metric ARIMA 
MAPE_ARIMA_M1 = regr.eval(test$Mean_Price,pricearimaforecasts1$mean)
MAPE_ARIMA_M1 = MAPE_ARIMA_M1[4]
MAPE_ARIMA_M1

MAPE_ARIMA_M2 = regr.eval(test$Mean_Price,pricearimaforecasts2$mean)
MAPE_ARIMA_M2 = MAPE_ARIMA_M2[4]
MAPE_ARIMA_M2

MAPE_ARIMA_M3 = regr.eval(test$Mean_Price,pricearimaforecast3$mean)
MAPE_ARIMA_M3 = MAPE_ARIMA_M3[4]
MAPE_ARIMA_M3

MAPE_ARIMA_M4 = regr.eval(test$Mean_Price,pricearimaforecast4$mean)
MAPE_ARIMA_M4 = MAPE_ARIMA_M4[4]
MAPE_ARIMA_M4

#10. Model building (Auto ARIMA) ====

MODEL_ARIMA = auto.arima(train_price, ic='aic')
summary(MODEL_ARIMA)

par(mfrow=c(1,1))
plot(MODEL_ARIMA$residuals)

# Box Ljung Test
set.seed(12334)
x = rnorm (100)
Box.test (x, lag = 1)
Box.test (x, lag = 1, type = "Ljung")

# Box test on our auto.arima model
Box.test(MODEL_ARIMA$residuals, lag = 10, type = "Ljung-Box")

# This statistic can be used to examine residuals from a time series 
# are not correlated in order to see if all underlying population    
# autocorrelations for the errors may be 0.
# Null hypothesis: error is not correlated
# Alt hypothesis: error is correlated

### Define the metric AUTO ARIMA
pricearimaforecasts_autArima= forecast(MODEL_ARIMA,h=4)
plot(pricearimaforecasts_autArima,flwd = 2)

MAPE_Auto_ARIMA = regr.eval(test$Mean_Price,pricearimaforecasts_autArima$mean)
MAPE_Auto_ARIMA = MAPE_Auto_ARIMA[4]
MAPE_Auto_ARIMA


#11. Model comparision ====

MAPE = c(MAPE_SMA,MAPE_WMA,MAPE_EMA,MAPE_HW,MAPE_ARIMA_M1,MAPE_ARIMA_M2,
         MAPE_ARIMA_M3,MAPE_ARIMA_M4,MAPE_Auto_ARIMA)
Models = c("SMA","WMA","EMA","HW","ARIMA_M1","ARIMA_M2",
           "ARIMA_M3","ARIMA_M4","Auto_ARIMA")

results = data.frame(Models,MAPE)
results
