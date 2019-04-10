rm(list=ls(all=TRUE))


setwd("C:/Users/Hai/Desktop/insofe/Week8/20180728_Bach46_CSE7305c_Lab01_Clustering_Lab01 (1)")

#Load all the Libraries Required
library(caret)
library(DMwR)


# Read the Data Set into R 
mydataset<-read.csv("Cereals.csv",header=TRUE,sep=",")
mydata = mydataset
str(mydata)
sum(is.na(mydata))
summary(mydata)
str(mydata)
#drop rating and name attributes
mydata$name=NULL
mydata$rating=NULL
mydata


ImputedData=centralImputation(mydata)
ImputedData <- scale(ImputedData) # standardize variables 
sum(is.na(ImputedData))



###-------------------------    Hierarchical Clustering     ------------------------###
# Ward's method 
# distance matrix euclidean
d <- dist(ImputedData,method = "euclidean") 

View(data.matrix(d))
fit <- hclust(d, method="ward.D") #WARD is a min variance method to find compact clusters

plot(fit) # display dendogram
fit$merge
fit$dist.method

groups <- cutree(fit, k=5) # cut tree into 5 clusters
groups

#groups <- cutree(fit, k=2) # cut tree into 2 clusters
#groups

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 
mydata_clusters=data.frame(ImputedData,groups)
par(mfrow=c(2,2))
fit1 <-hclust(d, method="complete")
fit2 <- hclust(d, method="single")
fit3 <- hclust(d, method='average')
fit4 <- hclust(d, method='ward.D')

############
dev.off()
par(mfrow = c(2, 2))
plot(fit3, leaflab = "textlike", main = "Average", xlab = "")
plot(fit4, leaflab = "textlike", main = "Ward", xlab = "")
plot(fit2, leaflab = "textlike", main = "Single", xlab = "")
plot(fit1, leaflab = "textlike", main = "Complete", xlab = "")

### Finding Optimum number of clusters
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(ImputedData, hcut, method = "wss")


##-------------------------    K- means Clustering     ------------------------###
# K-Means Cluster Analysis with k = 5
set.seed(123)
fit <- kmeans(ImputedData, 5) # 5 cluster solution
fit$withinss
fit$betweenss
#study the mdoel
fit$cluster
fit$tot.withinss
fit
fit$centers

#or # get cluster means
#aggregate(mydata,by=list(fit$cluster),FUN=mean)
#dev.off()

#install.packages("factoextra")
library(factoextra)
fviz_cluster(fit, ImputedData)

# append cluster label to the actual data frame
ImputedData <- data.frame(ImputedData, fit$cluster)
#write.csv(mydata,"kmeans_2.csv")
head(ImputedData)

### Finding Optimum number of clusters
# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(ImputedData,centers=i)$withinss)
}

#the scree plot
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 
#Choose that k, where the within groups sum of sqaures converges 

#Using factoextra library
factoextra::fviz_nbclust(ImputedData[,-c(13)], kmeans, method = "wss")
#k=6!?
set.seed(123)
final_fit_kmeans <- kmeans(ImputedData, 6) # 6 cluster solution

###-------------------------  on unseen data   ------------------------###
# For unseen data, we compute its distance from all the cluster centroids
# and assigns it to that cluster that is nearest to it

test_datapoint <- ImputedData[sample(1:nrow(ImputedData),1),]
closest.cluster <- function(x) {
  cluster.dist <- apply(fit$centers, 1, function(y) sqrt(sum((x-y)^2)))
  print(cluster.dist)
  return(which.min(cluster.dist)[1])
}

closest.cluster(test_datapoint)


###-------------------------  stability check   ------------------------###
#stabilitycheck
set.seed(123)
index <- (sample(nrow(ImputedData),.70*nrow(ImputedData)))
data <- ImputedData[index,]
fit2 <- kmeans(data,5)
data$clusters <- fit2$cluster

group1 <- ImputedData$fit.cluster[index]
group2 <- data$clusters

#loop dis for n imes. 
#install.packages("fossil")
library(fossil)
stabilitycheck <- adj.rand.index(group1, group2)
stabilitycheck
#across samples: avg_stabilitycheck
#Index value between 0 and 1, where 1 means the two clustering outcomes match identically.


# install.packages("clusteval")
library(clusteval)
Stabindex <- cluster_similarity(group1, group2, similarity = "jaccard", method="independence")
Stabindex

# Final Analysis
library(cluster)
library(fpc)
ImputedData$name = mydataset$name
par(mfrow=c(1,1))
clusplot(ImputedData[,-c(14)], ImputedData$fit.cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

