# Problem Statement
# This dataset contains information collected by the U.S Census Service concerning
# housing in Boston Mass. Predict the price of house(MEDV).
# Data Set
# You are provided with a csv file- "housing.csv" And description of the attributes in
# the "Housing Data Descp.txt"
# Evaluations
# Consider all the relevant metrics required to evaluate this problem

### Clear environment
rm(list=ls(all=TRUE))

# Set directory and read the data 
setwd("C:/Users/Hai/Desktop/insofe/Week6/20180714_Batch46_CSE7302c_Multiple_Lin_Reg_Lab")
data<-read.csv("Housing.csv",header=T)
str(data)
summary(data)

rows=seq(1,nrow(data),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(data))/100)
train = data[trainRows,] 
test = data[-trainRows,] 

sum(is.na(train))
sum(is.na(test))

# Model1- Build model with all attributes into model 
LinReg1<- lm(MEDV ~ ., data=train)
summary(LinReg1)
#Not significant INDUS,AGE
#Residual analysis
par(mfrow=c(2,2))
#372,373,369 --influential  381--cooks dis

##Model Performance Evaluation
#Error verification on train data
library(DMwR)
regr.eval(train$MEDV, LinReg1$fitted.values) 
#mape--
# mae        mse       rmse       mape 
# 3.3373280 22.9728276  4.7929978  0.1659358 
#Error verification on test data
Pred<-predict(LinReg1,test)
regr.eval(test$MEDV, Pred)
# mae        mse       rmse       mape 
# 3.4527833 20.5312317  4.5311402  0.1821914


#Model2
#Not significant INDUS,AGE

# Model2 - Build model with significant attributes
LinReg3<-lm(MEDV~.-INDUS,data=train)
summary(LinReg3)

LinReg2<-lm(MEDV~.-INDUS-AGE,data=train)
summary(LinReg2)

# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
regr.eval(train$MEDV, LinReg2$fitted.values) 
# mae       mse      rmse      mape 
# 3.339968 23.017704  4.797677  0.166012 
#Error verification on test data
Pred<-predict(LinReg2,test)
regr.eval(test$MEDV, Pred)
# mae        mse       rmse       mape
# 3.4467045 20.3926536  4.5158226  0.1818969

## Standardizing the Data
library(caret)
# The "preProcess()" function creates a model object required for standardizing unseen data
# Do not standardize the target variable

train_nonstd = train
test_nonstd = test

independentattr<-setdiff(names(train),c("MEDV"))
std_model <- preProcess(train[, independentattr], method = c("range"))

# The predict() function is used to standardize any other unseen data

train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
test[, independentattr] <- predict(object = std_model, newdata = test[, independentattr])


# Model3- Build linear regression with all standardized attributes 
LinReg_std1<-lm(MEDV~., data=train)
summary(LinReg_std1)

#Error verification on train data
regr.eval(train$MEDV, LinReg_std1$fitted.values) 

#Error verification on test data
Pred<-predict(LinReg_std1,test)
regr.eval(test$MEDV, Pred)
# mae        mse       rmse       mape 
# 3.4527833 20.5312317  4.5311402  0.1821914 
plot(LinReg_std1)



# Check for multicollinearity and perform dimensionality reduction analysis
library(corrplot)
cor(train[,-c(1,11,12)])
#Model4 - Build Model based on VIF Analysis
# 1. VIF: (check attributes with high VIF value)
library(car)
vif(LinReg_std1)
# CRIM       ZN    INDUS     CHAS      NOX       RM      AGE      DIS      RAD      TAX  PTRATIO 
# 1.795621 2.139730 3.847624 1.089707 4.420403 1.910591 3.192219 4.026246 7.241636 8.667529 1.790208 
# B    LSTAT 
# 1.301807 2.910120 
#all are lessthan 10

# 2. Stepwise Regression
#Model5 - Build Model based on Stepwise Regression
library(MASS)
Step1 <- stepAIC(LinReg_std1, direction="backward")
#Step2 <- stepAIC(LinReg1, direction="forward")
Step3 <- stepAIC(LinReg_std1, direction="both")
summary(Step3)
Mass_LinReg1 <- lm(MEDV~+CRIM+ZN+B+TAX+CHAS+NOX+RAD+PTRATIO+DIS+RM+LSTAT,data=train)

summary(Mass_LinReg1)
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))
head(train)

#Model6 - Build model without the influencial point (record #369) 
which(rownames(train)%in%c(369))

train[189,]
LinReg_No_infl<- lm(MEDV ~ ., data=train[-189,])
summary(LinReg_No_infl)

#Another Way to remove Influential Variables
#cooks distance
cook = cooks.distance(Mass_LinReg1)
plot(cook,ylab="Cooks distances")
max=as.numeric(which.max(cook))
points(max,cook[max],col='red', pch=19)
train[max,]
train <- train[-max,]


#Error verification on train data
regr.eval(train$MEDV, Mass_LinReg1$fitted.values) 
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,test)
regr.eval(test$MEDV, MASS_Pred1)

Error_calc1 = data.frame(train$MEDV,Mass_LinReg1$fitted.values)
write.csv(x = Error_calc1,file = "Error_calc1.csv")



#Model7 - Build model with Variable Transformations on Y and/or X variables


plot(MEDV ~ ., data = data)
dataForModel <- data
#Y Variable Transformation
dataForModel$MEDV <- log(dataForModel$MEDV)

# split the data into train and test data sets
rows=seq(1,nrow(dataForModel),1)
set.seed(123)
trainRows=sample(rows,(70*nrow(dataForModel))/100)
train = dataForModel[trainRows,]
test = dataForModel[-trainRows,]

LinReg5<- lm(MEDV ~ ., data=train)
summary(LinReg5)
par(mfrow=c(2,2))
plot(LinReg5)
plot(LinReg5,which=4)
hist(resid(LinReg5))

#Error verification on train data
library(DMwR)
regr.eval(exp(train$MEDV), exp(LinReg5$fitted.values))
#Error verification on test data
Pred<-predict(LinReg5,test)
regr.eval(exp(test$MEDV), exp(Pred))
#Future value prediction
temp <- test[14,]
temp$MEDV<- NULL
predict(LinReg1,temp)

##

