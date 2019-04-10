Objective: Predict the net hourly electrical energy output (EP) of the plant.

Machine Learning model: Simple Linear Regression

Data Set Information: The dataset contains 9568 data points collected from a Combined Cycle Power Plant over 6 years (2006-2011), when the power plant was set to work with full load.

Attribute Information: Features consist of hourly average ambient variables - Temperature (T) in the range 1.81°C and 37.11°C, - Ambient Pressure (AP) in the range 992.89-1033.30 milibar, - Relative Humidity (RH) in the range 25.56% to 100.16% - Exhaust Vacuum (V) in teh range 25.36-81.56 cm Hg - Net hourly electrical energy output (EP) 420.26-495.76 MW The averages are taken from various sensors located around the plant that record the ambient variables every second.
rm(list = ls(all = TRUE))
getwd()
setwd("C:/Users/Hai/Documents/insofe/Week5")


# Create two different datasets with the given dataset.
# Dataset1: Check for missing values and remove them, if it does not contain any missing values
# keep the dataset as it is.


Dataset2: Standardize Dataset1 except Target variable (Use z score)
Dataset1 <- read.csv("PredictEnergy1 (1).csv", header = T,  na.strings = c(NA,"?",""))
str(Dataset1)
View(Dataset1)
sum(is.na(Dataset1)) #no missing values
Data_NumAtr<-subset(Dataset1,select=c(AT,V,AP,RH))
library(DMwR)
library(vegan)
dataStd. = decostand(Data_NumAtr,"standardize")
summary(dataStd.)
StdDataset2=cbind(dataStd.,PE=Dataset1$PE)


# 1. Split in to train and validation sets. (use same seed)
seed(1234)
trainrows=sample(1:nrow(StdDataset2),round(0.7*nrow(StdDataset2),0))
trainrows

traindata<-StdDataset2[trainrows,]
dim(traindata)
validationdata<-StdDataset2[-trainrows,]
dim(validationdata)


# 2. Built models with every pair of Independent and dependent variables on each dataset.
# 3. Calculate the error metric for both train and validation sets.

names(traindata)
#Model1
#PE--AT
m1<-lm(PE~AT,data=traindata)
summary(m1)
# rsq=0.8995
#Residual standard error: 5.404 on 6696 degrees of freedom
traindata$AT[1]
traindata$PE[1]
m1$residuals[1]
#pe hat
m1$fitted.values[1]
#all residuals
traindata$AT-m1$fitted.values


trainpreds1<-predict(m1,traindata)
trainpreds1[1]
m1$fitted.values[1]
valpreds1<-predict(m1,validationdata)
regr.eval(traindata$PE,trainpreds1)
regr.eval(validationdata$PE,valpreds1)

actuals=traindata$PE

sqrt(sum((actuals-trainpreds1)^2)/(nrow(traindata)-1))
#5.403254

#model2
#PE--V
m2<-lm(PE~V,data=traindata)
summary(m2)
# Residual standard error: 8.438 on 6696 degrees of freedom
# Multiple R-squared:  0.7549
traindata$V[1]
traindata$PE[1]
m2$residuals[1]
#PE hat
m2$fitted.values[1]
#all residuals
traindata$V-m2$fitted.values


trainpreds2<-predict(m2,traindata)
trainpreds2[1]
m2$fitted.values[1]
valpreds2<-predict(m2,validationdata)
regr.eval(traindata$PE,trainpreds2)
regr.eval(validationdata$PE,valpreds2)

actuals=traindata$PE

sqrt(sum((actuals-trainpreds2)^2)/(nrow(traindata)-1))

#8.437353

#model3
#PE--AP
m3<-lm(PE~AP,data=traindata)
summary(m3)
# Residual standard error: 14.62 on 6696 degrees of freedom
# Multiple R-squared:  0.2639
traindata$AP[1]
traindata$PE[1]
m3$residuals[1]
#PE hat
m3$fitted.values[1]
#all residuals
traindata$AP-m3$fitted.values


trainpreds3<-predict(m3,traindata)
trainpreds3[1]
m3$fitted.values[1]
valpreds3<-predict(m3,validationdata)
regr.eval(traindata$PE,trainpreds3)
regr.eval(validationdata$PE,valpreds3)

actuals=traindata$PE

sqrt(sum((actuals-trainpreds3)^2)/(nrow(traindata)-1))

# 14.62151
#model4
#PE--AP
m4<-lm(PE~RH,data=traindata)
summary(m4)
# Residual standard error: 15.72 on 6696 degrees of freedom
# Multiple R-squared:  0.1494,
traindata$RH[1]
traindata$PE[1]
m4$residuals[1]
#PE hat
m4$fitted.values[1]
#all residuals
traindata$AP-m4$fitted.values


trainpreds4<-predict(m4,traindata)
trainpreds4[1]
m4$fitted.values[1]
valpreds4<-predict(m4,validationdata)
regr.eval(traindata$PE,trainpreds3)
regr.eval(validationdata$PE,valpreds4)

actuals=traindata$PE

sqrt(sum((actuals-trainpreds4)^2)/(nrow(traindata)-1))

# 15.71766



# 4. Observe which variable is showing more impact on the target.
# 5. Understand the regression equation for each model.

#m1 gives least rmse value with high rsq:AT shows more impact on target PE