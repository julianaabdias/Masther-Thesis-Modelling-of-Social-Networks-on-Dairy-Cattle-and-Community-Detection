##### load data
df <- read.table("C:/Users/its/Desktop/Ilias/Courses/Statistical learning/Assignment 3/five-personality.txt", sep = "\t", 
                 header = TRUE, colClasses="numeric")
##### omit missing values
df <- na.omit(df)
##### random sample
set.seed(1)
train <- sample(1:nrow(df), 5000)
train <- df[train,]
#### chech if random sample makes sense
library(dplyr)
compare_them <- function(data1,data2) {
  sum1 <- apply(data1,2,summary) %>% data.frame() 
  sum2 <- apply(data2,2,summary) %>% data.frame() 
  
  names(sum1) <- paste0(names(sum1),"1")
  names(sum2) <- paste0(names(sum2),"2")
  
  final <- cbind(sum1,sum2)
  
  final1 <- t(final) 
  
  final2 <- final1[order(row.names(final1)), ]
  
  final_1 <- t(final2) %>% data.frame()
  final_1
}
compare_them(df,train) %>% View()
#### k-means clustering
#### Calculating optimal number of clusters 
# Calculating optimal number of clusters (silouette plot)
library(factoextra)
library(NbClust)
fviz_nbclust(train, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
#### Plotting model quality vs. number of clusters
fviz_nbclust(train, kmeans, method = "wss")
#### K-means clustering
set.seed(5)
km.out <- kmeans(train , 5 , nstart =20)
km.out$centers
# which(km.out$centers[1,]-km.out$centers[2,] > 1)
# which(km.out$centers[1,]-km.out$centers[2,] < -1)
five_clustrs <- as.data.frame(apply(km.out$centers, 2, function(x) which.max(x)))
cluster_1 <- which(five_clustrs$`apply(km.out$centers, 2, function(x) which.max(x))` ==1)
cluster_2<- which(five_clustrs$`apply(km.out$centers, 2, function(x) which.max(x))` ==2)
cluster_3 <- which(five_clustrs$`apply(km.out$centers, 2, function(x) which.max(x))` ==3)
cluster_4 <- which(five_clustrs$`apply(km.out$centers, 2, function(x) which.max(x))` ==4)
cluster_5 <- which(five_clustrs$`apply(km.out$centers, 2, function(x) which.max(x))` ==5)
############################################################################################
pr.out <- prcomp(train)
comp <- data.frame(pr.out$x[,1:9])
x11()
plot(comp$PC1,comp$PC2, col = km.out$cluster, pch=16)
#### check if they give same results
table(cutree(hclust(dist(train), method ="complete"), 2), km.out$cluster)
#### make prediction based on the function before
set.seed(123)
train2 <- sample(1:nrow(df), 1000)
train2 <- df[train2,]
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}
aa <- clusters(train2, km.out[["centers"]])
#### compare if they make the same clusters
set.seed(2)
km.out2 <- kmeans(train2 , 5 , nstart =20)
table(km.out2$cluster,aa)
