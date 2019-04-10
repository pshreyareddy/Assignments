## clear the environment 
rm(list=ls(all=TRUE))

##load required libraries
library(recommenderlab)
library(reshape2)
library(ggplot2)
setwd("C:/Users/Hai/Desktop/insofe/Week9/20180812_Batch46_CSE7305c_CF_Lab03")


# Read data file along with header
tr<-read.csv("jester_ratings.csv",header=TRUE)


# Understand the data
head(tr)
str(tr)
summary(tr)



## Cat the datframe into user-rating mtrix
g<-acast(tr, UserID~ ItemID)

# Check the class of g
class(g)

# Convert g into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(g, "realRatingMatrix")
r

# view r in other possible ways
as(r, "list")     # A list
as(r, "matrix")   # A sparse matrix

# you can turn it into data-frame
head(as(r, "data.frame"))

# normalize the rating matrix
r_m <- normalize(r)
r_m
as(r_m, "list")

#Split the data into train and evaluation sets

min(rowCounts(r))

e <- evaluationScheme(r, method="split", train=0.6,given=1)

getRatingMatrix(getData(e,"train"))
getRatingMatrix(getData(e,"known"))
getRatingMatrix(getData(e,"unknown"))


# Create a recommender object (model)
#UBCF: User-based collaborative filtering
#IBCF: Item-based collaborative filtering
#Parameter 'method' decides similarity measure
#Cosine or Jaccard

##Build UBCF
r1 <- Recommender(getData(e, "train"), method = "UBCF")  
r1
getModel(r1)

#Predict UBCF
p1 <- predict(r1, getData(e, "known"), type="ratings")
p1
as(p1, "list")
calcPredictionAccuracy(p1, getData(e, "unknown"))




