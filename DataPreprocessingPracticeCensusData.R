setwd("C:/Users/Hai/Downloads/20180707_Batch46_CSE7212c_RLab04 (1)/20180707_Batch46_CSE7212c_RLab04 (1)")

# 1. The datasets "Census Income Data Set1.csv" & "Census Income Data Set2.csv" contain the census data. "Census Income Data Set1.csv" contains the demographics data and "Census Income Data Set2.csv" contains other details. The details are provided in the text file "CensusIncomeDataDesciption".
# a) Import the "Census Income Data" files into R( Consider ?, "" as missing values)

CIDSet1 <- read.csv("Census Income Data Set1.csv", header = T,  na.strings = c(NA,"?",""))
CIDSet2 <- read.csv("Census Income Data Set2.csv", header = T,  na.strings = c(NA,"?",""))
# b) Check the structure of both the files, Merge these two datasets to get the final dataset to analyze and write this file in to csv in your current working directory

str(CIDSet1)
str(CIDSet2)
# $ PersonID
# $ PID

mergeddata=merge(x=CIDSet1, y=CIDSet2, by.x="PersonID", by.y="PID")
write.csv(mergeddata,"mergeddata.csv", row.names=F)
View(mergeddata)
str(mergeddata)
# c) Understand the data and perform required pre-processing steps.
# I. Structure and summary of the data
str(mergeddata)
summary(mergeddata)
# II. Find Number of missing values and drop a row if it contains more than 20% of columns contains missing values. Impute the remining.
sum(is.na(mergeddata))
sum(is.na(ExcludedMissing))
library(DMwR)
length(manyNAs(mergeddata, 0.2) )
ExcludedMissing= mergeddata[!rowSums(is.na(mergeddata)) > ncol(mergeddata)*.2,]
sum(is.na(ExcludedMissing))
ImputedData=centralImputation(ExcludedMissing)

# III. Changing and recoding the factor levels for the following attribute "Class" to a new variable "outcome" based on "Class" if Class" <=50K" then outcome = 1 else outcome =0
levels(ImputedData$class)
table(ImputedData$class)
ImputedData$outcome = as.factor(ifelse(ImputedData$class == " <=50K", 1, 0))
table(ImputedData$outcome)

# d) Identify the numeric and categorical attributes accordingly. Split the data frame into two data frames each of numerical and categorical attributes respectively.
str(ImputedData)
numericData = ImputedData[which(lapply(ImputedData,is.numeric) == 'TRUE')]
categoricalData = ImputedData[which(lapply(ImputedData,is.factor) == 'TRUE')]


# e) Standardize Numerical attributes using range method.
library(vegan)
dataStd. <- decostand(numericData ,"range") 
summary(dataStd.)

# f) Split the Data in to train, test(70:30)
trainrows=sample(1:nrow(ImputedData),round(0.7*nrow(ImputedData),0))


train<-ImputedData[trainrows,]
dim(train)
test<-ImputedData[-trainrows,]
dim(test)
# g) Create a dummy variables for attribute "occupation".
library(dummies)
DummyVars<-dummy(ImputedData$occupation)
head(DummyVars)
data<-data.frame(ImputedData,DummyVars)
head(data)

# h) Find the average age of Male and Female who got divorced from Merged Data

agemean=ImputedData%>%group_by(gender)%>%summarise(mean(age))





