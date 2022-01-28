#Home Assingment 3

#cleaning the environment
rm(list=ls())


#Problem 1: This problem is about investigating some inferential properties of the 
#linear regression model. 

### a) Estimate the model parameters using the simulated data. 
###You can do this by running lm(y~x), in R. 
###Now take the variables in deviation form. 
###This can be done in R as flows: u<- x-mean(x) v<- y-mean(y) 
###Now run a linear model of v and u as lm(v~u). 
###Do you expect the slope parameters of these two regression models 
###(y on x, and v on u) to be exactly the same? Explain why.   

#Create simulated data with 100 observations
# coefficients B0 = -1, B1 = 2
# predictor xi ~ N(0,1) and ei ~ N(0,1)
set.seed(100)
x <- rnorm(100)
e <- rnorm(100)
y <- -1 + 2 * x + e
mean(x)
mean(y)

boxplot(y)
hist(y)

#Estimating model parameters using simulated data
lmfit <- (lm(y~x))
summary(lmfit)

#Now take the variables in deviation form
u<- x-mean(x) 
v<- y-mean(y) 

#Now run a linear model of v and u
lmfitmean <- lm(v~u)
summary(lmfitmean)

#plots
par(mfrow = c(1, 2))
#plot lm fit
par(mar = c(8, 8,1.5,1.5))
plot(x,y, pch=19, ylab= "Y", xlab = "X", cex.lab=1)
#add linear regression fitted line
abline(lmfit, col="red", lwd= 2)
abline(v=0, col="gray", lwd=2)
abline(v=mean(x), col="black", lwd=2)
abline(h=mean(y), col="blue", lwd=2)

#plot centered data and new lm
par(mar = c(8, 8,1.5,1.5))
plot(u,v, pch=19, ylab= "Mean centered Y", xlab = "Mean centered X", cex.lab=1)
#add linear regression fitted line
abline(lmfitmean, col="red", lwd= 2)
abline(v=0, col="gray", lwd=2)
abline(v=mean(u), col="black", lwd=2)
abline(h=mean(v), col="blue", lwd=2)



# b) Because the intercept parameter in the regression of v on u is 0, 
#better you run this regression by omitting the intercept term, e.g. run lm(v~u+0). 
#Also run a reverse regression i.e. lm(u~v+0). Notice that, you get exactly 
#the same t-statistic in the both models. Is this a coincidence or did you expect 
#it? Explain with a mathematical proof that the equality of the t-statistic is 
#expected, or disprove by showing a counter example.

#run the regression by omitting the intercept term
data2 <- data.frame(u,v)
summary(lm(v~u+0))

#Also run a reverse regression 
summary(lm(u~v+0))

###################################################################################

library(RColorBrewer)
library(scales)
library(rgl)
library(tidyverse)

#####Problem 2
#In this exercise, you are asked to perform clustering in the five_personality 
#dataset and create clusters that classify people based on their responses 
#to the quiz. Since the dataset is quite large, you should do sampling of an 
#appropriate subset and built the clusters based on this subset. 
#Try to validate the clusters by assigning appropriate labels and 
#visualizing the results. 
#Check if kmeans clustering and hierarchical clustering provide the same clusters. 

#setting the working directory to where the csv file is saved
setwd("C:/Users/julia/Documents/Master BI/Statistical Learning/Home Assignment/Home Assingment 3")

#Import csv file into R
dataframe <- read.delim('five-personality.txt', 
                        stringsAsFactors = FALSE, blank.lines.skip = TRUE)

#Get to know the data frame
head(dataframe)
summary(dataframe)
View(dataframe)
dim(dataframe)

#delete all NA values, since dataset is large enough and it seems NA values
#are at the end of data, as it was an error when reading the file more than 
#actually missing values
dataframe <- na.omit(dataframe)
dim(dataframe)

#select subset of data as dataset is too large
#number selected according to what wcc code could handle
set.seed(3)
train <- sample(1:nrow(dataframe),15000, replace = FALSE)
clusterdata <- dataframe[train,]
View(clusterdata)
summary(clusterdata)
dim(clusterdata)


# Calculating optimal number of clusters 
wss <- 0
for(i in 1:20) {
  
  km.output <- kmeans(clusterdata, centers = i, nstart = 20, iter.max = 50 )
  wss[i] <- km.output$tot.withinss
}
summary(km.output)

#Plotting model quality vs. number of clusters
plot(1:20, wss, type="b", col="red", xlab="Number of clusters", ylab = "WSS",
     main = "Optimal Number of Clusters - WSS)")


# Calculating optimal number of clusters 
library(factoextra)
library(NbClust)

set.seed(123)
par(mfrow = c(1, 2))
fviz_nbclust(clusterdata, kmeans, method = "wss")

fviz_nbclust(clusterdata, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


#number of clusters between 4 and 6 according to Wss method
#Run the Kmeans, initially with 4 clusters
set.seed(1)
k4 <- kmeans(clusterdata, 4, nstart = 20)
k4$cluster
k4

#since there are more than 2 predictors, perform PCA and plot the first 
#two principal components score vectors. 
pca_cluster <- prcomp(clusterdata, scale = FALSE)
summary(pca_cluster)
pca_cluster$x
data_cluster_pca <- data.frame(pca_cluster$x[,1:8])
x11()
plot(data_cluster_pca, col=k4$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k4$cluster, pch = 16,
     main = "Plot PC1 x PC2 - with 4 clusters")


#Run the Kmeans with 2 clusters according to Silhouete method
set.seed(1)
k2 <- kmeans(clusterdata, 2, nstart = 20)
k2$cluster
k2

#plot the first two principal components score vectors and 2 clusters 
x11()
plot(data_cluster_pca, col=k2$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k2$cluster, pch = 16,
     main = "Plot PC1 x PC2 - with 2 clusters")


#Run the Kmeans with 5 clusters
set.seed(1)
k5 <- kmeans(clusterdata, 5, nstart = 20)
k5$cluster
k5

#plot the first two principal components score vectors and 5 clusters 
x11()
plot(data_cluster_pca, col=k5$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k5$cluster, pch = 16,
     main = "Plot PC1 x PC2 - with 5 clusters")

#Run the Kmeans with 6 clusters
set.seed(1)
k6 <- kmeans(clusterdata, 6, nstart = 20)
k6$cluster
k6

#plot the first two principal components score vectors and 6 clusters 
x11()
plot(data_cluster_pca, col=k6$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k6$cluster, pch = 16,
     main = "Plot PC1 x PC2 - with 6 clusters")

#label the clusters - 2 clusters
#first create a new data set with all columns
newdata <- clusterdata[1:50]
View(newdata)


#create columns to consolidate scores according to the types of personality
newdata$EXT_Outgoing <- (newdata$EXT1 + newdata$EXT3 + newdata$EXT5 +
                                 newdata$EXT7 + newdata$EXT9)/5
newdata$EXT_Solitary <- (newdata$EXT2 + newdata$EXT4 + newdata$EXT6 +
                         newdata$EXT8 + newdata$EXT10)/5

newdata$OPN_Inventive <- (newdata$OPN1 + newdata$OPN3 + newdata$OPN5 + newdata$OPN7 +
                            newdata$OPN8 + newdata$OPN10)/6
newdata$OPN_Consistent <- (newdata$OPN2 + newdata$OPN4 + newdata$OPN6 +
                           newdata$OPN9)/4

newdata$CSN_Organized <- (newdata$CSN1 + newdata$CSN3 + newdata$CSN5 +
                            newdata$CSN7+ newdata$CSN9 + newdata$CSN10)/6
newdata$CSN_Careless <- (newdata$CSN2 + newdata$CSN4 + newdata$CSN6 +
                           newdata$CSN8)/4

newdata$AGR_Friendly <- (newdata$AGR2 + newdata$AGR4 + newdata$AGR6 +
                           newdata$AGR8 + newdata$AGR9 + newdata$AGR10)/6
newdata$AGR_Challenging <- (newdata$AGR1 + newdata$AGR3 + newdata$AGR5 +
                              newdata$AGR7)/4

newdata$EST_Nervous <- (newdata$EST1 + newdata$EST3 + newdata$EST5 +
                          newdata$EST6 + newdata$EST7+ newdata$EST8 +
                          newdata$EST9 + newdata$EST10)/8 
newdata$EST_Confident <- (newdata$EST2 + newdata$EST4)/2

#include column with the classification of clusters
newdata$Cluster <- k2$cluster

#create subset with new columns and cluster column
subset_2clusters <- newdata %>% select(51:61)
summary(subset_2clusters)

#calculate means of variables in order to label clusters
dat <- data.frame(subset_2clusters %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean"))

dat

View(dat)

#label the clusters - 4 clusters

#include column with the classification of clusters - 4 clusters
newdata$Cluster <- k4$cluster

#create subset with new columns and cluster column
subset_4clusters <- newdata %>% select(51:61)
summary(subset_4clusters)

#calculate means of variables in order to label clusters
dat2 <- data.frame(subset_4clusters %>%
                    mutate(Cluster = k4$cluster) %>%
                    group_by(Cluster) %>%
                    summarise_all("mean"))

View(dat2)


####### - Hierarchical Clustering
dd <- dist(clusterdata)
hc.complete =hclust(dist(clusterdata), method="complete")
hc.average =hclust(dist(clusterdata), method ="average") 
hc.single=hclust(dist(clusterdata), method ="single")

#complete method with Eucledian Distance
x11()
plot(hc.complete, 
     main="Complete Linkage with Eucledian Distance", xlab="", sub="", cex=.9)
abline (h=25, col =" red ")

plot(hc.average, 
     main="Average Linkage with Eucledian Distance", xlab="", sub="", cex=.9)
plot(hc.single, 
     main="Single Linkage with Eucledian Distance", xlab="", sub="", cex=.9)

#Use correlation-based distance
data1 <- scale(clusterdata)
ddc = as.dist(1- cor(t(data1)))
hcor.complete =hclust(ddc, method="complete")
hcor.average =hclust(ddc, method ="average") 
hcor.single=hclust(ddc, method ="single")

#plot Complete Linkage
x11()
hc_correlation <- plot(hcor.complete, main=" Complete Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=1.81, col =" red ")
hcor.complete.clusters <- cutree(hcor.complete,2)
hcor.complete.clusters

#plot Average Linkage
x11()
hc_correlation2 <- plot(hcor.average, main=" Average Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=0.997, col =" red ")

#plot Single Linkage
x11()
hc_correlation3 <- plot(hcor.single, main=" Single Linkage
with Correlation -Based Distance ", xlab="", sub ="")

#label the clusters - 2 clusters
#first create a new data set with all columns
newdata <- clusterdata[1:50]
View(newdata)


#create columns to consolidate scores according to the types of personality
newdata$EXT_Outgoing <- (newdata$EXT1 + newdata$EXT3 + newdata$EXT5 +
                           newdata$EXT7 + newdata$EXT9)/5
newdata$EXT_Solitary <- (newdata$EXT2 + newdata$EXT4 + newdata$EXT6 +
                           newdata$EXT8 + newdata$EXT10)/5

newdata$OPN_Inventive <- (newdata$OPN1 + newdata$OPN3 + newdata$OPN5 + newdata$OPN7 +
                            newdata$OPN8 + newdata$OPN10)/6
newdata$OPN_Consistent <- (newdata$OPN2 + newdata$OPN4 + newdata$OPN6 +
                             newdata$OPN9)/4

newdata$CSN_Organized <- (newdata$CSN1 + newdata$CSN3 + newdata$CSN5 +
                            newdata$CSN7+ newdata$CSN9 + newdata$CSN10)/6
newdata$CSN_Careless <- (newdata$CSN2 + newdata$CSN4 + newdata$CSN6 +
                           newdata$CSN8)/4

newdata$AGR_Friendly <- (newdata$AGR2 + newdata$AGR4 + newdata$AGR6 +
                           newdata$AGR8 + newdata$AGR9 + newdata$AGR10)/6
newdata$AGR_Challenging <- (newdata$AGR1 + newdata$AGR3 + newdata$AGR5 +
                              newdata$AGR7)/4

newdata$EST_Nervous <- (newdata$EST1 + newdata$EST3 + newdata$EST5 +
                          newdata$EST6 + newdata$EST7+ newdata$EST8 +
                          newdata$EST9 + newdata$EST10)/8 
newdata$EST_Confident <- (newdata$EST2 + newdata$EST4)/2

#include column with the classification of clusters
newdata$Cluster <- hcor.complete.clusters

#create subset with new columns and cluster column
subset_3clusters <- newdata %>% select(51:61)
summary(subset_3clusters)

#calculate means of variables in order to label clusters
dat3 <- data.frame(subset_3clusters %>%
                    mutate(Cluster = hcor.complete.clusters) %>%
                    group_by(Cluster) %>%
                    summarise_all("mean"))

dat3

View(dat3)



#Finally, take a different subset and make predictions on where the observations 
#of the second dataset are clustered based on the algorithms you have built in 
#the first subset. 

#select subset of data as dataset is too large
#number selected according to what K-means code could handle
set.seed(1)
test <- sample(1:nrow(dataframe),15000)
testdata <- dataframe[test,]
View(testdata)
summary(testdata)

#create columns to consolidate scores according to the types of personality
testdata$EXT_Outgoing <- (testdata$EXT1 + testdata$EXT3 + testdata$EXT5 +
                           testdata$EXT7 + testdata$EXT9)/5
testdata$EXT_Solitary <- (testdata$EXT2 + testdata$EXT4 + testdata$EXT6 +
                           testdata$EXT8 + testdata$EXT10)/5

testdata$OPN_Inventive <- (testdata$OPN1 + testdata$OPN3 + testdata$OPN5 + 
                             testdata$OPN7 + testdata$OPN8 + testdata$OPN10)/6
testdata$OPN_Consistent <- (testdata$OPN2 + testdata$OPN4 + testdata$OPN6 +
                             testdata$OPN9)/4

testdata$CSN_Organized <- (testdata$CSN1 + testdata$CSN3 + testdata$CSN5 +
                            testdata$CSN7+ testdata$CSN9 + testdata$CSN10)/6
testdata$CSN_Careless <- (testdata$CSN2 + testdata$CSN4 + testdata$CSN6 +
                           testdata$CSN8)/4

testdata$AGR_Friendly <- (testdata$AGR2 + testdata$AGR4 + testdata$AGR6 +
                           testdata$AGR8 + testdata$AGR9 + testdata$AGR10)/6
testdata$AGR_Challenging <- (testdata$AGR1 + testdata$AGR3 + testdata$AGR5 +
                              testdata$AGR7)/4

testdata$EST_Nervous <- (testdata$EST1 + testdata$EST3 + testdata$EST5 +
                          testdata$EST6 + testdata$EST7+ testdata$EST8 +
                          testdata$EST9 + testdata$EST10)/8 
testdata$EST_Confident <- (testdata$EST2 + testdata$EST4)/2 


#create subset with new columns and cluster column
subset_testdata <- testdata %>% select(51:60)
summary(subset_testdata)

#Apply K-means with 2 clusters
set.seed(1)
k2_testdata <- kmeans(subset_testdata, 2, nstart = 20)
k2_testdata$cluster
k2_testdata

#since there are more than 2 predictors, perform PCA and plot the first 
#two principal components score vectors. 
test_pca_cluster <- prcomp(subset_testdata, scale = FALSE)
summary(test_pca_cluster)
test_pca_cluster$x
test_data_cluster_pca <- data.frame(test_pca_cluster$x[,1:6])

x11()
plot(test_data_cluster_pca, col=k2_testdata$cluster, pch=16)
plot(test_data_cluster_pca$PC1, test_data_cluster_pca$PC2, col=k2_testdata$cluster, pch = 16)


#Apply K-means with 4 clusters
set.seed(1)
k4_testdata <- kmeans(subset_testdata, 4, nstart = 20)
k4_testdata$cluster
k4_testdata


#plot the first two principal components score vectors and 4 clusters 
x11()
plot(testdata_cluster_pca, col=k4_testdata$cluster, pch=16)
plot(test_data_cluster_pca$PC1, test_data_cluster_pca$PC2, col=k4_testdata$cluster, pch = 16)


#include column with the classification of 2 clusters
subset_testdata$Cluster <- k2_testdata$cluster
View(subset_testdata)

#calculate means of variables in order to label clusters
label_cluster2 <- data.frame(subset_testdata %>%
                     mutate(Cluster = k2_testdata$cluster) %>%
                     group_by(Cluster) %>%
                     summarise_all("mean"))

View(label_cluster2)

#include column with the classification of 4 clusters
subset2_testdata <- subset_testdata
subset2_testdata$Cluster <- k4_testdata$cluster
View(subset2_testdata)

#calculate means of variables in order to label clusters
label_cluster4 <- data.frame(subset2_testdata %>%
                               mutate(Cluster = k4_testdata$cluster) %>%
                               group_by(Cluster) %>%
                               summarise_all("mean"))

View(label_cluster4)
