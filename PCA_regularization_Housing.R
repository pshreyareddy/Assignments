
#Clean Your Global Environment
# Problem Statement
# This dataset contains information collected by the U.S Census Service concerning
# housing in Boston Mass. Predict the price of house(MEDV).
#Do PCA and then apply lasso with gaussian compare results with MLR
rm(list=ls(all=TRUE))

#Load all the Libraries Required

library(glmnet)
library(caret)
library(DMwR)



# Read the Data Set into R 
setwd("C:/Users/Hai/Desktop/insofe/Week7/PCA_RegularizationAssignment")

data<-read.csv("Housing.csv",header=TRUE,sep=",")


##Check the structure of the dataset
# * Observing the structure will reveal what are the data types of attributes
# * It can be helpful to understand any data type changes are required

str(data)

##Check the summary of the dataset

summary(data)


## Remove columns which does not add any information

x=nearZeroVar(data,names=TRUE)

data=data[,setdiff(names(data),x)]


##Check for Missing Values

sum(is.na(data))



##Split data into train and val

set.seed(125)
rows=createDataPartition(data$MEDV,p = 0.7,list = FALSE)
train=data[rows,]
val=data[-rows,]

###Apply standardizations

pre1<-preProcess(train[,setdiff(colnames(train),"MEDV")])
train_scale<-predict(pre1,train[,setdiff(colnames(train),"MEDV")])
val_scale<-predict(pre1,val[,setdiff(colnames(val),"MEDV")])



# Running a PCA using princomp function only on independent varaibles
prcomp_train <- princomp(train_scale)

plot(prcomp_train)
names(prcomp_train)
prcomp_train$loadings



summary(prcomp_train)


# Setting the threshold and storing the component names for future use
#0.90 culmulate proportion
comp_Names = c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5","Comp.6")

# transformed data in new components
train_data<-prcomp_train$scores

# get only highest variation components and bind target
train_data<-data.frame(train_data[,comp_Names],"MEDV"=train$MEDV)

# apply same transformation on test
val_data<-predict(prcomp_train,val_scale)

#subset the components only which are in train_data and bind target 
val_data<-data.frame(val_data[,comp_Names],"MEDV"=val$MEDV)

# model=lm(MEDV~.,data=train_data)
# summary(model)
# #Adjusted R:0.6842
# model1 = lm(MEDV~.,data=train)
# summary(model1)
# #Adjusted Rsq:0.7233
# #error metrics on train_data
# preds<-predict(model,train_data)
# preds_val<-predict(model,val_data)
# 
# preds1<-predict(model1,train)
# preds_val1<-predict(model1,val)

library(DMwR)
regr.eval(train_data$MEDV,preds)
# mae        mse       rmse       mape 
# 3.3615362 25.3751421  5.0373745  0.1761881 
regr.eval(val_data$MEDV,val)
# mae          mse         rmse         mape 
# 932.94936 293855.72850    542.08461     49.93338 
regr.eval(train$MEDV,preds1)
# mae        mse       rmse       mape 
# 3.2159266 21.7862636  4.6675758  0.1633436 
# > 
regr.eval(val$MEDV,preds_val1)

# mae       mse      rmse      mape 
# 3.412936 23.646204  4.862736  0.163116 
### Model Building 

### LASSO Regression
# 1. Let us build a simple Lasso  regression
# 2. Lets do a cross validation with Lasso  regression
# 3. Also Will tune the Model for perfect "lamda value"
x.train=as.matrix(train_data[,setdiff(names(train_data),"MEDV")])
y.train=train_data$MEDV
x.val = as.matrix(val_data[,setdiff(names(val_data),"MEDV")])
y.val = val$MEDV
#test=as.matrix(test)
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)

fit.lasso.cv <- cv.glmnet(x.train, y.train, type.measure="class", alpha=1, 
                          family="gaussian",nfolds=5,parallel=TRUE)

plot(fit.lasso, xvar="lambda")
plot(fit.lasso.cv)

coef(fit.lasso.cv,s = fit.lasso.cv$lambda.min)
pred.lasso.cv.train <- predict(fit.lasso.cv,x.train,s = fit.lasso.cv$lambda.min,type="response")
pred.lasso.cv.val <- predict(fit.lasso.cv,x.val,s = fit.lasso.cv$lambda.min,type="response")

regr.eval(pred.lasso.cv.train, y.train)
# mae        mse       rmse       mape 
# 3.3622300 25.3960318  5.0394476  0.2918027
regr.eval(pred.lasso.cv.val, y.val)
# mae        mse       rmse       mape 
# 3.3775349 26.3888904  5.1370118  0.2812563 





#-----------------------------
# Model1- Build model with all attributes into model 
LinReg1<- lm(MEDV ~ ., data=train)
summary(LinReg1)
# Adjusted R-squared:  0.7233 
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
Pred<-predict(LinReg1,val)
regr.eval(val$MEDV, Pred)
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
Pred<-predict(LinReg2,val)
regr.eval(val$MEDV, Pred)
# mae        mse       rmse       mape
# 3.4467045 20.3926536  4.5158226  0.1818969

## Standardizing the Data
library(caret)
# The "preProcess()" function creates a model object required for standardizing unseen data
# Do not standardize the target variable

train_nonstd = train
test_nonstd = val

independentattr<-setdiff(names(train),c("MEDV"))
std_model <- preProcess(train[, independentattr], method = c("range"))

# The predict() function is used to standardize any other unseen data

train[, independentattr] <- predict(object = std_model, newdata = train[, independentattr])
val[, independentattr] <- predict(object = std_model, newdata = val[, independentattr])


# Model3- Build linear regression with all standardized attributes 
LinReg_std1<-lm(MEDV~., data=train)
summary(LinReg_std1)

#Error verification on train data
regr.eval(train$MEDV, LinReg_std1$fitted.values) 
# mae        mse       rmse       mape 
# 3.2159266 21.7862636  4.6675758  0.1633436 
#Error verification on test data
Pred<-predict(LinReg_std1,val)
regr.eval(val$MEDV, Pred)
# mae       mse      rmse      mape 
# 3.412936 23.646204  4.862736  0.163116 
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
# Adjusted R-squared:  0.7238 
Mass_LinReg1 <- lm(formula = MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + 
                     TAX + PTRATIO + B + LSTAT, data = train)

summary(Mass_LinReg1)
# Adjusted R-squared:  0.7238 
par(mfrow=c(2,2))
plot(Mass_LinReg1)
plot(Mass_LinReg1,which=4)
par(mfrow=c(1,1))
head(train)

#Model6 - Build model without the influencial point (record #369) 
which(rownames(train)%in%c(369))

train[256,]
LinReg_No_infl<- lm(MEDV ~ ., data=train[-256,])
summary(LinReg_No_infl)
# Adjusted R-squared:  0.7444 
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
# mae        mse       rmse       mape 
# 3.4631722 23.8297759  4.8815751  0.1833722 
#Error verification on test data
MASS_Pred1<-predict(Mass_LinReg1,val)
regr.eval(val$MEDV, MASS_Pred1)
# mae        mse       rmse       mape 
# 3.3645712 23.0850476  4.8046902  0.1612989 
#MLR mape is less than PCA and Lasso

