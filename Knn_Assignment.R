rm(list=(ls(all=TRUE)))
library(caret)
setwd("C:/Users/Hai/Desktop/insofe/Week9/20180812_Batch46_CSE7305c_KNN_Lab03")

# 1.	Read the given data "UniversalBank.csv" and check the structure,dimensions 

flightdata<-read.csv("FlightDelays.csv",sep=",")
dim(flightdata)
str(flightdata)
summary(flightdata)
sum(is.na(flightdata))

# 2.	Drop the duplicate records if you have any. (optional)

flightdata<-flightdata[!duplicated(flightdata),]

# 3. Type cast the columns.
flightdata$DAY_WEEK=as.factor(flightdata$DAY_WEEK)
flightdata$Flight.Status=as.factor(flightdata$Flight.Status)


#5. Train and val split
set.seed(123) # to get same data in each time
rows= createDataPartition(flightdata$Flight.Status,p=0.7,list=FALSE) 
train = flightdata[rows,] 
val = flightdata[-rows,] 
str(train)

#7. Create Dummies(dummifies categorical variables)
dumvar=dummyVars(Flight.Status~.,data=train)

train_target=train$Flight.Status
val_target=val$Flight.Status
train=predict(dumvar,train)
val=predict(dumvar,val)

head(train)   #no target
#8. Applying KNN
library(class)

#Deciding k value for k-NN

#Experiment with various odd values of k; k={1,3,5,7,..}

# k = 1
noOfNeigh <- 1
pred = knn(train, val,train_target, k = noOfNeigh)
a = confusionMatrix(pred,val_target,positve="1")
a



#k = 3

noOfNeigh <- 3
pred = knn(train, val,train_target, k = noOfNeigh)
a = confusionMatrix(pred,val_target,positive="1")
a

# k = 5
noOfNeigh <- 5
pred = knn(train, val,train_target, k = noOfNeigh)
a = confusionMatrix(pred,val_target,positive="1")
a

# k = 9
noOfNeigh <- 9
pred = knn(train, val,train_target, k = noOfNeigh)
a = confusionMatrix(pred,val_target,positive="1")
a
# k = 11
noOfNeigh <- 11
pred = knn(train, val,train_target, k = noOfNeigh)
a = confusionMatrix(pred,val_target,positive="1")
a

# k = 15
noOfNeigh <- 15
pred = knn(train, val,train_target, k = noOfNeigh)
a = confusionMatrix(pred,val_target,positive="1")
a


# Speeding up knn

### Condensing the data 
#To reduce the complexity of the model data is condensed
#To understand drop=FALSE parameter have a look at http://adv-r.had.co.nz/Subsetting.html

#condensing the number of records to compute distances from a test record 
keep = condense(train, train_target) # gives row numbers only
length(keep)
keep[1:10]
#Run the model on condensed data
pred = knn(train[keep,], val, 
           train_target[keep],k=15)
a = confusionMatrix(pred,val_target)
a


### 9. Knn with cross validation and Grid search from library caret

trctrl <- trainControl(method = "cv", number = 5)
set.seed(3333)
grid <- expand.grid(k=c(3,5,7,9,13,15))
knn_fit <- train(train,train_target, method = "knn",
                 trControl=trctrl,
                 tuneGrid=grid)
plot(knn_fit)

pred = predict(knn_fit,newdata = val)
a = confusionMatrix(pred,val_target)
a

### 10. 

## Applying FNN (Fastest NN)

library(FNN)
pred=FNN::knn(train[keep,], val, 
              train_target[keep],k=5, algorithm = 'kd_tree')

a = confusionMatrix(pred,val_target)
a

#


