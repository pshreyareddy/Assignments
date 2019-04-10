

# Cleaning the work space
rm(list = ls(all=TRUE))
# Setting the working directory
## Read the Data
# Make sure the dataset is located in your current working directory, 
# else you can change your working directory using the "setwd()" function.
setwd("C:/Users/Hai/Desktop/insofe/Week6/LogisticRegression_Naive_Bayes_Lab03/Lab3")
players_data <- read.csv("players.csv", header=T, sep=",")
head(players_data)
View(players_data)
## Understand the data
# Use the str() function to get the dimensions and types of attributes in the dataset
str(players_data)

# Use the summary() function to understand the distribution of variables in the dataset
summary(players_data)

# Use the head() and tail() functions to get a look at the data

head(players_data)
tail(players_data)

#Convert TargetVariable into factor
players_data$TARGET_5Yrs = as.factor(players_data$TARGET_5Yrs)
str(players_data)
View(players_data)
summary(players_data)
# Data Pre-processing
## Missing Values
#  Check the number of missing values in the data frame
sum(is.na(players_data))

library(DMwR)

ExcludedMissing= players_data[!rowSums(is.na(players_data)) > ncol(players_data)*.2,]
sum(is.na(ExcludedMissing))
ImputedData=centralImputation(ExcludedMissing)
Player_Data=ImputedData
str(Player_Data)

# Dropping the Name column as it has too many levels.
Player_Data$Name = NULL
str(Player_Data)

## Train/Validation Test Split
# * Split the data 70/20/10 into train,validation and test sets by using __Stratified Sampling
# Split the data using stratified sampling, 
# we can do that by using the createDataPartition() function from the caret package

library(caret)
set.seed(786)
# The argument "y" to the createDataPartition() function is the response variable
# The argument "p" is the percentage of data that goes to training
# The argument "list" should be input a boolean (T or F). Remember to put list = F, else the output is going to  be a list and your data can't be subsetted with it
table(Player_Data$TARGET_5Yrs)
train_rows <- createDataPartition(Player_Data$TARGET_5Yrs, p = 0.7, list = F)
train_data <- Player_Data[train_rows, ]
Set2<-Player_Data[-train_rows, ]
valrows<-createDataPartition(Set2$TARGET_5Yrs, p = 0.7, list = F)
validationdata<-Set2[valrows,]
test_data <- Set2[-valrows, ]

#str(train_data)
#sanity check: see the dimensions of the set 
dim(train_data)
dim(validationdata)
dim(test_data)
dim(Player_Data)

#Sanity check Check the proportions of classes in all datasets with the given dataset
prop.table(table(train_data$TARGET_5Yrs))
prop.table(table(validationdata$TARGET_5Yrs))
prop.table(table(test_data$TARGET_5Yrs))
prop.table(table(Player_Data$TARGET_5Yrs))




# Build a model
## Basic Logistic Regression Model
# Use the glm() function to build a basic model
# Build a model using all the variables, excluding the response variable, in the dataset,family is binomial

log_reg <- glm(TARGET_5Yrs ~ ., data = train_data, family = "binomial")

# Get the summary of the model and understand the output
summary(log_reg)

# Assume it is the base model check accuracy on triandata
# predict the probabilities of train data make it to 0 if it is less than<0.5,otherwise 1
# creating confusion matrix
#check the accuracy
trainprobs<-predict(log_reg,train_data,type="response")
trainprobs[1]
#trainp<-predict(log_reg,train_data)
#trainprobs[1]
#trainp[1]
#1/(1+exp(2.220446e-16 ))
#p1<-predict(log_reg,train_data)
trainpreds<-ifelse((trainprobs<0.5), "0", "1")

cnf<-table(train_data$TARGET_5Yrs,trainpreds)#actuals,predictions
trainacc<-sum(diag(cnf))/sum(cnf)
s <- cnf[2,2]/sum(cnf[2,])
print(trainacc)

# Do the same on validation data
valprobs<-predict(log_reg,validationdata,type="response")
valpreds<-ifelse((valprobs<0.5), "0", "1")
cnf<-table(validationdata$TARGET_5Yrs,valpreds)
valacc<-sum(diag(cnf))/sum(cnf)
print(valacc)


## stepAIC model
# "stepAIC()" is a function in the MASS package
# * stepAIC uses AIC (Akaike information criterion) to either drop variables ("backward" direction) or add variables ("forward" direction) from the model

library(MASS)
m<-stepAIC(log_reg)

summary(m)
m$call

#see the ouput of step AIC : Use only the ouput variables for model building

logistic_stepaic<-glm(formula = TARGET_5Yrs ~ GP + MIN + PTS + X3P.Made + X3PA + 
                        FT. + DREB + REB + AST + BLK + TOV, family = "binomial", 
                      data = train_data)


# Predict and check the accuracy on train data
prob_train<-predict(logistic_stepaic,train_data,type="response")
trainpreds<-ifelse(prob_train<0.5,"0", "1")
# creating confusion matrix
cnf<-table(train_data$TARGET_5Yrs,trainpreds)
# check the accuracy 
trainacc<-sum(diag(cnf))/sum(cnf)
print(trainacc)

# Check on validatin data
prob_val<-predict(logistic_stepaic,validationdata,type="response")
valpreds<-ifelse((prob_val<0.5), "0", "1")
cnf<-table(validationdata$TARGET_5Yrs,valpreds)
testacc<-sum(diag(cnf))/sum(cnf)
print(testacc)

## Modifying the Model with the VIF - **Variance Inflation Factor :**

# We use the "vif()" function from the car package. 
# Every explanatory variable would have a VIF score
# A VIF > 10 means that there are signs of multi-collinearity and anything greater than 10 means that an explanatory variable can be dropped

library(car)
vif(logistic_stepaic)
# experiment by droping variables VIF>10
# GP       MIN       PTS  X3P.Made      X3PA       FT.      DREB       REB       AST       BLK 
# 1.426672 11.961771  7.611370 28.570013 30.265108  1.281453 27.588579 29.565113  4.198283  1.778685 
# TOV 
# 5.434953 

logistic_stepaic1<-glm(formula = TARGET_5Yrs ~ GP + MIN + PTS + X3P.Made + 
                        FT. + DREB + REB + AST + BLK + TOV, family = "binomial", 
                      data = train_data)
vif(logistic_stepaic1)
logistic_stepaic1<-glm(formula = TARGET_5Yrs ~ GP + MIN + PTS + X3P.Made + 
                         FT. + DREB + AST + BLK + TOV, family = "binomial", 
                       data = train_data)
vif(logistic_stepaic1)
logistic_stepaic1<-glm(formula = TARGET_5Yrs ~ GP + PTS + X3P.Made + 
                         FT. + DREB + AST + BLK + TOV, family = "binomial", 
                       data = train_data)
vif(logistic_stepaic1)


# ROC-determining Cutoff probability(we have used default 0.5 till now)
# Hence we must first choose a cutoff point for getting to the original levels of the response variables
# To choose the cutoff point we will use the traindata,test data should not be used to make any decisions regarding the model

## Creating an ROC plot
# __Steps to create an ROC plot :__
# 1. Get a list of predictions (probability scores) using the predict() function, Use the argument 'type = "response"' in the predict function to get a list of predictions between 0 and 1
# By default if no dataset is mentioned, training data is used


# Using the ROCR package create a "prediction()" object
library(ROCR)
# The prediction object takes the probability scores and the original levels for theses data as input

prob_train<-predict(logistic_stepaic1,train_data,type="response")
rocpreds <- prediction(prob_train, train_data$TARGET_5Yrs)
table(train_data$TARGET_5Yrs)
#slotNames(rocpreds)

# The prediction object contains a list of 
# 1.predictions (probability scores),
# 2.original class labels, 3.cutoffs,4. false positives,5. true positives, 6. true negatives, 7.false negatives, 
# 8.No. of positive predictions and No. of negative predictions corresponding to these cutoffs. 
#Class distribution in the dataset.



# Extract performance measures (True Positive Rate and False Positive Rate) using the "performance()" function from the ROCR package
# The performance() function from the ROCR package helps us extract metrics such as True positive rate, False positive rate etc. from the prediction object, we created above.
# Two measures (y-axis = tpr, x-axis = fpr) are extracted

perf <- performance(rocpreds, measure="tpr", x.measure="fpr")
slotNames(perf)
perf@x.name
perf@y.name
perf@x.values
# Plot the ROC curve using the extracted performance measures (TPR and FPR)
par(mfrow=c(1,1))
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))


# Extract the AUC score of the ROC curve and store it in a variable named "auc"
# Use the performance() function on the prediction object created above using the ROCR package, to extract the AUC score

perf_auc <- performance(rocpreds,  measure="auc")

# Access the auc score from the performance object

auc <- perf_auc@y.values[[1]]

print(auc)

# For different threshold values identifying the tpr and fpr
perf@alpha.values
perf@alpha.name
perf@y.values
# For different threshold values identifying the tpr and fpr
cutoffs <- data.frame(cut= perf@alpha.values[[1]], fpr= perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])

# Sorting the data frame in the decreasing order based on tpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(cutoffs)
# Plotting the true positive rate and false negative rate based on the cutoff       
# increasing from 0.05-0.1
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
class(perf)


## Choose a Cutoff Value
# Based on the trade off between TPR and FPR depending on the business domain, a call on the cutoff has to be made.
# A cutoff of 0.6 can be chosen which is in conservative area

# Predictions on train -->
pred_class <- ifelse(prob_train> 0.6, "1", "0")
table(train_data$TARGET_5Yrs,pred_class)

## Predictions on validation data
# After choosing a cutoff value of 0.1, let's predict the class labels on the test data using our model
prob_val <- predict(log_reg, validationdata, type = "response")
preds_val <- ifelse(prob_val > 0.6, "1", "0")

# Evaluation Metrics for classification
## Manual Computation
### Confusion Matrix

conf_matrix <- table(validationdata$TARGET_5Yrs, preds_val)
print(conf_matrix)

### Specificity
# The Proportion of correctly identified negatives by the test/model.

specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
print(specificity)

### Sensitivity

# The Proportion of correctly identified positives by the test/model.
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
print(sensitivity)

### Accuracy
# The Proportion of correctly identified psotivies/negatives in the entire population by the test/model

accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
print(accuracy)



prob_test <- predict(log_reg, test_data, type = "response")
preds_test <- ifelse(prob_test > 0.6, "1", "0")
cnf<-table(test_data$TARGET_5Yrs,preds_test)
accuracy <- sum(diag(cnf))/sum(cnf)
print(accuracy)








