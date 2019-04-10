rm(list=ls())

library(xgboost)
library(caret)
library(dplyr)
library(DMwR)

data = read.csv("Housing.csv",header = TRUE,sep = ',')
str(data)
sum(is.na(data))

set.seed(1234)
rows <- createDataPartition(data$MEDV, p = 0.7, list = F)

train <- data[rows, ]
test <- data[-rows, ]

std <- preProcess(train[,!(names(train) %in% "MEDV")])
View(train_data)
train_data <- predict(std,train)
test_data <- predict(std, test)

train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, !(names(train_data) %in% c("MEDV"))]), 
                            label = as.matrix(train_data[, names(train_data) %in% "MEDV"]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, !(names(test_data) %in% c("MEDV"))]), 
                           label = as.matrix(test_data[, names(test_data) %in% "MEDV"]))

#basic xgboost model
xgb_basic <- xgboost(data = train_matrix, max.depth = 10, 
                     eta = 0.5, nthread = 2, nround = 500, 
                     objective = "reg:linear", verbose = 1, 
                     early_stopping_rounds = 10)
basic_preds <- predict(xgb_basic, test_matrix)
regr.eval(test$MEDV, basic_preds)


# Build an XGBoost Model with parameters
params_list <- list("objective" = "reg:linear",
                    "eta" = 0.3,
                    "early_stopping_rounds" = 10,
                    "max_depth" = 10,
                    "gamma" = 0.5,
                    "colsample_bytree" = 0.6,
                    "subsample" = 0.65,
                    "eval_metric" = "rmse",
                    "silent" = 1)

xgb_with_params <- xgboost(data = train_matrix, params = params_list, 
                           nrounds = 500, early_stopping_rounds = 20)

params_preds <- predict(xgb_with_params, test_matrix)
regr.eval(test$MEDV, params_preds)


###Variable Importance
var_imp_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_with_params)
xgb.plot.importance(var_imp_matrix)


