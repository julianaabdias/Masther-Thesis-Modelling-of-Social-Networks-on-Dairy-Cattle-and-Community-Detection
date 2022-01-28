#Home Assingment 2

#cleaning the environment
rm(list=ls())

#loading libraries
library(MASS)
library(tree)
library(ISLR)
library(randomForest)
library(e1071)

#setting the working directory to where the csv file is saved
getwd()
setwd("C:/Users/julia/Documents/Master BI/Statistical Learning/Home Assignment/Home Assingment 2")

#Import csv file into R
dataframe <- read.csv(file='Data_Cortex_Nuclear.csv')

#Read the first 6 elements of each variable and the summary
head(dataframe)
summary(dataframe)

#View the whole dataframe
dim(dataframe)
View(dataframe)

#create a dataframe without the variable MouseID, which won't add any information to the
#models
data <- dataframe[,c(2:82)]
dim(data)
table(data$class)



#check how many NA values there are in the data set
sum(is.na(data))

#Handling missing values, substitute NA values by 1/2 of minimum values
for (i in 1:77) {
  data[is.na(data[,i]),i] <- min(data[,i], na.rm = TRUE)/2
}

#check if the NA values were correctly substituted
sum(is.na(data))

#############Question 1
##a) Use the 77 proteins as predictors for decision trees 
## and support vector machines models to make binary and multiple class classification.

#Split the data set into a training set (70%) and a test set (30%)
dim(data)
1080*0.70
set.seed(3)
train <- sample(1:nrow(data), 750)
test <- data[-train,]

#select the dataset for multiple classification
data_mclass <- data[,c(1:77,81)]

#select the dataset for binary classification
data_bclass <- data[,c(1:78)]


#################PART a.1: Fit Decision Tree with all predictors - Binary Classification
tree_bclass <- tree(as.factor(Genotype) ~ ., data_bclass[train,])
summary(tree_bclass)
#plot tree
x11()
plot(tree_bclass)
text(tree_bclass, pretty = 0)
#predictions
tree_bclass_pred <- predict(tree_bclass, newdata = data_bclass[-train,], type = "class")
table(tree_bclass_pred, data_bclass[-train,]$Genotype)
#evaluate % of correct predictions in test set
class_accuracy <- (161+124)/330
class_accuracy
#test error
1-class_accuracy
#apply cross validation in order to choose the best numbe of trees
cv_tree_bclass <- cv.tree(tree_bclass, FUN = prune.misclass)
cv_tree_bclass
#elbow plot to find the best number of trees
par(mfrow = c(1,2))
plot(cv_tree_bclass$size, cv_tree_bclass$dev, type='b')
plot(cv_tree_bclass$k,cv_tree_bclass$dev, type='b')
#prune tree
#The tree with 14 terminal nodes results in the lowest cross-validation error rate
#with 73 cross-validation errors.
prune_bclass <- prune.misclass(tree_bclass, best = 14) 
plot(prune_bclass)
text(prune_bclass, pretty = 0)
#predictions on pruned tree
tree_bclass_pred2 <- predict(prune_bclass, newdata = data_bclass[-train,], type = "class")
#confusion matrix
table(tree_bclass_pred2, data_bclass[-train,]$Genotype)
#classi???cation accuracy
length(data_bclass[-train,]$Genotype)
class_accuracy <- (156+126)/330
class_accuracy
#test error
1-class_accuracy

################PART a.2: SVM with all predictors - Binary Classification

###SVM Radial - Binary Classification
svm_radial_binary <- svm(as.factor(Genotype)~., data=data_bclass[train,], 
                         kernel ="radial", gamma = 1,cost = 1)
summary(svm_radial_binary)
#prediction
svm_binary_radial_pred <- predict(svm_radial_binary,
                              newdata = data_bclass[-train,])
#confusion table
table(predict = svm_binary_radial_pred , true= data_bclass[-train,]$Genotype)
#classi???cation accuracy
class_accuracy<- (183+12)/330
class_accuracy
#test error
1-class_accuracy
#cross-validation to select the best choice of ?? and cost for an SVM with a 
#radial kernel
set.seed(1) 
tuneout_svmradial_binary = tune(svm , as.factor(Genotype) ~., data = data_bclass, 
                kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                             gamma=c(0.5,1,2,3,4)))
summary(tuneout_svmradial_binary)
#prediction
pred_binary_radial <- predict(tuneout_svmradial_binary$best.model,
                              newdata = data_bclass[-train,])
#confusion table
table(predict = pred_binary_radial , true= data_bclass[-train,]$Genotype)
#classi???cation accuracy
class_accuracy<- (183+147)/330
class_accuracy
#test error
1-class_accuracy


###SVM Polynomial - Binary Classification
svm_poly_binary <- svm(as.factor(Genotype)~., data=data_bclass[train,], 
                         kernel ="polynomial", gamma = 1,cost = 1, degree = 2)
summary(svm_poly_binary)
#prediction
binary_poly_pred <- predict(svm_poly_binary,
                            newdata = data_bclass[-train,])

#confusion table
table(predict = binary_poly_pred , true= data_bclass[-train,]$Genotype)
#classi???cation accuracy
class_accuracy <- (183+147)/330
class_accuracy
#test error
1-class_accuracy

#cross-validation to select the best choice of degree, Y and cost for an SVM with a 
#polynomial kernel
set.seed(1) 
tuneout_svmpoly_binary = tune(svm , as.factor(Genotype) ~., data = data_bclass, 
                                kernel="polynomial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                                             gamma=c(0.5,1,2,3,4), 
                                                             degree=c(2,3,4)))
summary(tuneout_svmpoly_binary)
#prediction
pred_binary_poly <- predict(tuneout_svmpoly_binary$best.model,
                              newdata = data_bclass[-train,])

#confusion table
table(predict = pred_binary_poly , true= data_bclass[-train,]$Genotype)
#classi???cation accuracy
class_accuracy <- (183+147)/330
class_accuracy
#test error
1-class_accuracy



##########PART a.3: Fit Decision Tree with all predictors - Multiple Class Classification
tree_mclass <- tree(as.factor(class) ~ ., data_mclass[train,])
summary(tree_mclass)
#plot tree
x11()
plot(tree_mclass)
text(tree_mclass, pretty = 0)
#predictions
tree_mclass_pred <- predict(tree_mclass, newdata = data_mclass[-train,], type = "class")
table(tree_mclass_pred, data_mclass[-train,]$class)
#evaluate % of correct predictions in test set
(39+24+40+41+36+21+37+32)/330
#test error
1-((39+24+40+41+36+21+37+32)/330)
#apply cross validation in order to choose the best numbe of trees
cv_tree_mclass <- cv.tree(tree_mclass, FUN = prune.misclass)
cv_tree_mclass
#prune tree
#The tree with 27 terminal nodes results in the lowest cross-validation error rate
#with 82 cross-validation errors.
prune_mclass <- prune.misclass(tree_mclass, best = 27) 
x11()
plot(prune_mclass)
text(prune_mclass, pretty = 0)
#predictions on pruned tree
tree_mclass_pred2 <- predict(prune_mclass, newdata = data_mclass[-train,], type = "class")
#confusion matrix
table(tree_mclass_pred2, data_mclass[-train,]$class)
#classi???cation accuracy
length(data_mclass[-train,]$class)
class_accuracy <- (39+24+40+41+36+21+37+32)/330
class_accuracy
#test error
1- class_accuracy

#######################PART a.4: SVM with all predictors - Multiple Class Classification

###Kernel = radial
svm_radial_multiple <- svm(as.factor(class)~., data=data_mclass[train,], 
                         kernel ="radial", gamma = 1,cost = 1)
summary(svm_radial_multiple)
#prediction
multiple_radial_pred <- predict(svm_radial_multiple,
                                newdata = data_mclass[-train,])
#confusion table
table(predict = multiple_radial_pred , true= data_mclass[-train,]$class)
#classi???cation accuracy
class_accuracy <- (43+1+10+2+1+0+2+3)/330
class_accuracy
#test error
1-class_accuracy

#cross-validation to select the best choice of ?? and cost for an SVM with a 
#radial kernel
set.seed(1) 
tuneout_svmradial_multiple = tune(svm , as.factor(class) ~., data = data_mclass, 
                                kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                                             gamma=c(0.5,1,2,3,4)))
summary(tuneout_svmradial_multiple)
#prediction
pred_multiple_radial <- predict(tuneout_svmradial_multiple$best.model,
                              newdata = data_mclass[-train,])
#confusion table
table(predict = pred_multiple_radial , true= data_mclass[-train,]$class)
#classi???cation accuracy
class_accuracy <- (43+47+46+47+41+31+41+34)/330
class_accuracy
#test error
1-class_accuracy


###Kernel = polynomial
set.seed(1) 
svm_poly_multiple <- svm(as.factor(class)~., data=data_mclass[train,], 
                       kernel ="polynomial", gamma = 1,cost = 1, degree = 2)
summary(svm_poly_multiple)
#prediction
multiple_poly_pred <- predict(svm_poly_multiple,
                              newdata = data_mclass[-train,])

#confusion table
table(predict = multiple_poly_pred , true= data_mclass[-train,]$class)
#classi???cation accuracy
class_accuracy <- (43+47+46+46+41+30+41+34)/330
class_accuracy
#test error
1-class_accuracy


#cross-validation to select the best choice of degree, Y and cost for an SVM with a
#polynomial kernel
tuneout_svmpoly_multiple = tune(svm , as.factor(class) ~., data = data_mclass, 
                              kernel="polynomial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                                               gamma=c(0.5,1,2,3,4), 
                                                               degree=c(2,3,4)))
summary(tuneout_svmpoly_multiple)
#prediction
pred_multiple_poly <- predict(tuneout_svmpoly_multiple$best.model,
                            newdata = data_mclass[-train,])

#confusion table
table(predict = pred_multiple_poly , true= data_mclass[-train,]$class)
#classi???cation accuracy
class_accuracy <- (43+47+46+47+41+31+41+34)/330
class_accuracy
#test error
1-class_accuracy


######################################################################################

#b) Perform principal component analysis on the 77 numerical features. 
#Use an appropriate number of principal components as predictors and perform the same 
#classification task. 

#I'm using the whole data and selecting just the numerical features
data_pca = data[,1:77]
pca = prcomp( data_pca, scale=TRUE)
names(pca)

#check summary to see how many PCs would be better to use, in this case it seems like
#using only 6 PCs represents 68% of total variance
summary(pca)

#create dataframe with the 6 PCs
pca_df <- data.frame(pca$x[,1:6])
dim(pca_df)

#joing variables for binary and multiple classification into the new dataframe
class <- data[,81]
genotype <- data[,78]

pcadata_class <- cbind(pca_df, class)
dim(pcadata_class)
head(pcadata_class)

pcadata_binary <- cbind(pca_df, genotype)
dim(pcadata_binary)
head(pcadata_binary)

#create train and test set from this dataframe
#Split the data set into a training set (70%) and a test set (30%)
1080*0.70
set.seed(3)
trainm <- sample(1:nrow(pcadata_class), 750)

trainb <- sample(1:nrow(pcadata_binary), 750)

#NOW PERFORME THE SAME CLASSIFICATIONS AS BEFORE
####PART B.1: Fit Decision Tree with selected PCs - Binary Classification
tree_bclass_with_pca <- tree(as.factor(genotype) ~ ., pcadata_binary[trainb,])
summary(tree_bclass_with_pca)
#plot tree
x11()
plot(tree_bclass_with_pca)
text(tree_bclass_with_pca, pretty = 0)
#predictions
bclass_pred_pca <- predict(tree_bclass_with_pca, newdata = pcadata_binary[-trainb,], 
                           type = "class")
table(bclass_pred_pca, pcadata_binary[-trainb,]$genotype)
#evaluate % of correct predictions in test set
length(pcadata_binary[-trainb,]$genotype)
(168+78)/330
#test error
1-((168+78)/330)
#apply cross validation in order to choose the best numbe of trees
cv_tree_bclass_pca <- cv.tree(tree_bclass_with_pca, FUN = prune.misclass)
cv_tree_bclass_pca
#prune tree
#The tree with 10 terminal nodes results in the lowest cross-validation error rate
#with 220 cross-validation errors.
prune_bclass_pca <- prune.misclass(tree_bclass_with_pca, best = 10) 
x11()
plot(prune_bclass_pca)
text(prune_bclass_pca, pretty = 0)
#predictions on pruned tree
tree_bclass_pred_pca <- predict(prune_bclass_pca, newdata = pcadata_binary[-trainb,],
                                type = "class")
#confusion matrix
table(tree_bclass_pred_pca, pcadata_binary[-trainb,]$genotype)
#classi???cation accuracy
class_accuracy <- (158+85)/330
class_accuracy
#test error
1-class_accuracy

#######PART B.2: SVM with selected PCs - Binary Classification

###SVM Radial - Binary Classification
svm_radial_binary_pca <- svm(as.factor(genotype)~., data=pcadata_binary[trainb,], 
                         kernel ="radial", gamma = 1,cost = 1)
summary(svm_radial_binary_pca)
#prediction
binary_radial_pca_pred <- predict(svm_radial_binary_pca,
                                  newdata = pcadata_binary[-trainb,])
#confusion table
table(predict = binary_radial_pca_pred , true= pcadata_binary[-trainb,]$genotype)
#classi???cation accuracy
class_accuracy<- (170+136)/330
class_accuracy
#test error
1-class_accuracy
#cross-validation to select the best choice of ?? and cost for an SVM with a 
#radial kernel
set.seed(1) 
tuneout_svmradial_binary_pca = tune(svm , as.factor(genotype) ~., data=pcadata_binary, 
                                kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                                             gamma=c(0.5,1,2,3,4)))
summary(tuneout_svmradial_binary_pca)
#prediction
pred_binary_radial_pca <- predict(tuneout_svmradial_binary_pca$best.model,
                              newdata = pcadata_binary[-trainb,])
#confusion table
table(predict = pred_binary_radial_pca , true= pcadata_binary[-trainb,]$genotype)
#classi???cation accuracy
class_accuracy<- (182+148)/330
class_accuracy
#test error
1-class_accuracy


###SVM Polynomial - Binary Classification
svm_poly_binary_pca <- svm(as.factor(genotype)~., data=pcadata_binary[trainb,], 
                       kernel ="polynomial", gamma = 1,cost = 1, degree = 2)
summary(svm_poly_binary_pca)
#prediction
binary_poly_pca_pred <- predict(svm_poly_binary_pca,
                                newdata = pcadata_binary[-trainb,])

#confusion table
table(predict = binary_poly_pca_pred , true= pcadata_binary[-trainb,]$genotype)
#classi???cation accuracy
class_accuracy <- (145+105)/330
class_accuracy
#test error
1-class_accuracy
#cross-validation to select the best choice of degree, Y and cost for an SVM with a 
#polynomial kernel
set.seed(1) 
tuneout_svmpoly_binary_pca = tune(svm , as.factor(genotype) ~., data = pcadata_binary, 
                              kernel="polynomial", ranges=list(cost=c(0.1,1,10), 
                                                               degree=c(2,3,4)))
summary(tuneout_svmpoly_binary_pca)
#prediction
pred_binary_poly_pca <- predict(tuneout_svmpoly_binary_pca$best.model,
                            newdata = pcadata_binary[-trainb,])

#confusion table
table(predict = pred_binary_poly_pca , true= pcadata_binary[-trainb,]$genotype)
#classi???cation accuracy
class_accuracy <- (171+122)/330
class_accuracy
#test error
1-class_accuracy


####PART B.3: Fit Decision Tree with selected PCs - Multiple Class Classification
tree_mclass_pca <- tree(as.factor(class) ~ ., pcadata_class[trainm,])
summary(tree_mclass_pca)
#plot tree
x11()
plot(tree_mclass_pca)
text(tree_mclass_pca, pretty = 0)
#predictions
tree_mclass_pred_pca <- predict(tree_mclass_pca, newdata = pcadata_class[-trainm,], 
                                type = "class")
table(tree_mclass_pred_pca, pcadata_class[-trainm,]$class)
#evaluate % of correct predictions in test set
(32+29+16+18+28+8+18+27)/330
#test error
1-((32+29+16+18+28+8+18+27)/330)
#apply cross validation in order to choose the best numbe of trees
cv_tree_mclass_pca <- cv.tree(tree_mclass_pca, FUN = prune.misclass)
cv_tree_mclass_pca
#prune tree
#The tree with 21 terminal nodes results in the lowest cross-validation error rate
#with 371 cross-validation errors.
prune_mclass_pca <- prune.misclass(tree_mclass_pca, best = 21) 
x11()
plot(prune_mclass_pca)
text(prune_mclass_pca, pretty = 0)
#predictions on pruned tree
tree_mclass_pred_pca_pruned <- predict(prune_mclass_pca, newdata = pcadata_class[-trainm,], 
                                type = "class")
#confusion matrix
table(tree_mclass_pred_pca_pruned, pcadata_class[-trainm,]$class)
#classi???cation accuracy
length(pcadata_class[-trainm,]$class)
class_accuracy <- (28+29+16+18+29+8+18+27)/330
class_accuracy
#test error
1- class_accuracy

####PART B.4: SVM with selected PCs - Multiple Class Classification

###Kernel = radial
svm_radial_multiple_pca <- svm(as.factor(class)~., data = pcadata_class[trainm,], 
                           kernel ="radial", gamma = 1,cost = 1)
summary(svm_radial_multiple_pca)
#prediction
multiple_radial_pca_pred <- predict(svm_radial_multiple_pca,
                                    newdata = pcadata_class[-trainm,])
#confusion table
table(predict = multiple_radial_pca_pred , true= pcadata_class[-trainm,]$class)
#classi???cation accuracy
class_accuracy <- (43+41+40+37+40+30+30+34)/330
class_accuracy
#test error
1-class_accuracy


#cross-validation to select the best choice of ?? and cost for an SVM with a 
#radial kernel
set.seed(1) 
tuneout_svmradial_multiple_pca = tune(svm , as.factor(class) ~., data = pcadata_class, 
                                  kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                                               gamma=c(0.5,1,2,3,4)))
summary(tuneout_svmradial_multiple_pca)
#prediction
pred_multiple_radial_pca <- predict(tuneout_svmradial_multiple_pca$best.model,
                                newdata = pcadata_class[-trainm,])
#confusion table
table(predict = pred_multiple_radial_pca , true= pcadata_class[-trainm,]$class)
#classi???cation accuracy
class_accuracy <- (43+47+45+46+40+30+40+34)/330
class_accuracy
#test error
1-class_accuracy

###Kernel = polynomial
set.seed(1) 
svm_poly_multiple_pca <- svm(as.factor(class)~., data= pcadata_class[trainm,], 
                         kernel ="polynomial", gamma = 1,cost = 1, degree = 2)
summary(svm_poly_multiple_pca)
#prediction
multiple_poly_pca_pred <- predict(svm_poly_multiple_pca,
                                  newdata = pcadata_class[-trainm,])

#confusion table
table(predict = multiple_poly_pca_pred, true= pcadata_class[-trainm,]$class)
#classi???cation accuracy
class_accuracy <- (35+28+32+26+31+22+15+29)/330
class_accuracy
#test error
1-class_accuracy

#cross-validation to select the best choice of degree, Y and cost for an SVM with a
#polynomial kernel
tuneout_svmpoly_multiple_pca = tune(svm , as.factor(class) ~., data = pcadata_class, 
                                kernel="polynomial", ranges=list(cost=c(0.1,1,10,100,1000), 
                                                                 degree=c(2,3,4)))
summary(tuneout_svmpoly_multiple_pca)
#prediction
pred_multiple_poly_pca <- predict(tuneout_svmpoly_multiple_pca$best.model,
                              newdata = pcadata_class[-trainm,])

#confusion table
table(predict = pred_multiple_poly_pca , true= pcadata_class[-trainm,]$class)
#classi???cation accuracy
class_accuracy <- (43+47+46+45+40+30+37+34)/330
class_accuracy
#test error
1-class_accuracy


#########################################################################################

#c) Using bagging, random forest, and boosting perform the same classification task. 
#Compare the results of the three methods. 

#select the dataset for multiple classification
data_mclass <- data[,c(1:77,81)]

#select the dataset for binary classification
data_bclass <- data[,c(1:78)]


###########Part C.1 - Bagging - Binary
set.seed(1)
bag_binary <- randomForest(as.factor(Genotype)~.,data= data_bclass[train,], 
                           mtry=77,importance=TRUE)
#print confusion matrix and estimate train error
bag_binary
importance(bag_binary)
#prediction
bag_pred_binary <- predict(bag_binary ,newdata = data_bclass[-train,])
plot(bag_pred_binary, data_bclass[-train,]$Genotype)
#confusion table
table(predict = bag_pred_binary , true= data_bclass[-train,]$Genotype)
#classi???cation accuracy
class_accuracy <- (179+142)/330
class_accuracy
#test error
1-class_accuracy

#########C.2 - Random Forest - Binary
set.seed(1)
rf_binary <- randomForest(as.factor(Genotype)~.,data= data_bclass[train,], 
                           mtry=38,importance=TRUE)
#print confusion matrix and estimate train error
rf_binary
importance(rf_binary)
#prediction
rf_pred_binary <- predict(rf_binary ,newdata = data_bclass[-train,])
plot(rf_pred_binary, data_bclass[-train,]$Genotype)
#confusion table
table(predict = rf_pred_binary , true= data_bclass[-train,]$Genotype)
#classi???cation accuracy
class_accuracy <- (180+143)/330
class_accuracy
#test error
1-class_accuracy


#########C.3 - Boosting - Binary
library(gbm)
install.packages("mlbench")
library(mlbench)
set.seed(1)
#recode the variable Genotype for binary response
data_bclass2 <- data_bclass
View(data_bclass2)
data_bclass2$Genotype <- ifelse(data_bclass2$Genotype =="Control",1,0)
#applying boost method
boost_binary = gbm(Genotype~., data= data_bclass2[train,], 
                   distribution = "bernoulli", n.trees = 5000, interaction.depth = 4,
                   verbose = F,shrinkage = 0.01)
summary(boost_binary)
#prediction
boost_pred_binary <- predict(boost_binary, newdata = data_bclass2[-train,], n.trees = 5000)
plot(boost_pred_binary, data_bclass2[-train,]$Genotype)
#confusion table
pred <- (boost_pred_binary - min(boost_pred_binary)) / (max(boost_pred_binary) - min(boost_pred_binary))
prob <- ifelse(pred <= 0.5,0,1)
table(prob, data_bclass2[-train,]$Genotype)
#classi???cation accuracy
class_accuracy <- (147+181)/330
class_accuracy
#test error
1-class_accuracy


###########C.4 - Bagging - Multiple Classification
set.seed(1)
bag_multiple <- randomForest(as.factor(class)~.,data= data_mclass[train,], 
                           mtry=77,importance=TRUE)
#print confusion matrix and estimate train error
bag_multiple
importance(bag_multiple)
#prediction
bag_pred_multiple <- predict(bag_multiple ,newdata = data_mclass[-train,])
plot(bag_pred_multiple, data_mclass[-train,]$class)
#confusion table
table(predict = bag_pred_multiple , true= data_mclass[-train,]$class)
#classi???cation accuracy
class_accuracy <- (43+45+44+46+40+30+39+34)/330
class_accuracy
#test error
1-class_accuracy

#########C.5 - Random Forest - Multiple Classification
set.seed(1)
rf_multiple <- randomForest(as.factor(class)~.,data= data_mclass[train,], 
                          mtry=38,importance=TRUE)
#print confusion matrix and estimate train error
rf_multiple
importance(rf_multiple)
#prediction
rf_pred_multiple <- predict(rf_multiple ,newdata = data_mclass[-train,])
plot(rf_pred_multiple, data_mclass[-train,]$class)
#confusion table
table(predict = rf_pred_multiple , true= data_mclass[-train,]$class)
#classi???cation accuracy
class_accuracy <- (43+47+45+46+40+30+40+34)/330
class_accuracy
#test error
1-class_accuracy

#########C.3 - Boosting - Multiple Classification
set.seed(1)
#applying boost method
boost_multiple = gbm(as.factor(class)~., data= data_mclass[train,], 
                   distribution = "multinomial", n.trees = 5000, interaction.depth = 4,
                   verbose = F,shrinkage = 0.01, cv.folds = 10)
summary(boost_multiple)
#prediction
boost_pred_multiple <- predict(boost_multiple, newdata = data_mclass[-train,], 
                               n.trees = 5000, type = 'response')
pred=as.matrix(boost_pred_multiple[,,1])
#confusion table
#The predicted result is not easy-readable data so we'll get class names 
#with the highest prediction value.
p.pred <- apply(pred, 1, which.max)
table(p.pred , true= data_mclass[-train,]$class)

#classi???cation accuracy
class_accuracy <- (43+47+46+47+41+31+40+34)/330
class_accuracy
. #test error
1-class_accuracy


######################################################################################

#2. Use the dataset to perform clustering. 
#You should try both k-means clustering and hierarchical clustering. 
#In every case, find a number of clusters that make sense and try to explain what each 
#cluster describes. 
library(RColorBrewer)
library(scales)
library(rgl)


#######2.1 - K-means
#define the data to be used for cluster, only numerical variables
set.seed(1)
clusterdata <- data[,1:77]
View(clusterdata)
summary(clusterdata)

#Run the Kmeans, initially with 2 clusters
set.seed(1)
k2 <- kmeans(clusterdata, 2, nstart = 20)
k2$cluster
k2

#since there are more than 2 predictors, perform PCA and plot the ???rst 
#two principal components score vectors. 
?prcomp
pca_cluster <- prcomp(clusterdata, scale = TRUE)
typeof(pca_cluster)
summary(pca_cluster)
pca_cluster$x
data_cluster_pca <- data.frame(pca_cluster$x[,1:6])
x11()
plot(data_cluster_pca, col=k2$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k2$cluster, pch = 16)

#plot PC1 and PC2 against the labels already existent in the dataset
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=as.factor(data$Genotype), 
     pch = 16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=as.factor(data$Treatment), 
     pch = 16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=as.factor(data$Behavior), 
     pch = 16)

#label the clusters
newdata <- data[1:80]
newdata$labels <- k2$cluster
table(newdata$Behavior,newdata$labels)
table(newdata$Treatment,newdata$labels)
table(newdata$Genotype,newdata$labels)

# Calculating optimal number of clusters 
library(factoextra)
set.seed(123)
fviz_nbclust(clusterdata, kmeans, method = "wss")

#Run the Kmeans, initially with 3 clusters
k3 <- kmeans(clusterdata, 3, nstart = 20)
str(k3)
k3$cluster
k3

#plot PC1 and PC2 with 3 clusters
x11()
plot(data_cluster_pca, col=k3$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k3$cluster, pch = 16)

#label the clusters
newdata <- data[1:80]
newdata$labels <- k3$cluster
table(newdata$Behavior,newdata$labels)
table(newdata$Treatment,newdata$labels)
table(newdata$Genotype,newdata$labels)

#Run the Kmeans with 4 clusters
k4 <- kmeans(clusterdata, 4, nstart = 20)
k4$cluster
k4

#plot PC1 and PC2 with 4 clusters
x11()
plot(data_cluster_pca, col=k4$cluster, pch=16)
plot(data_cluster_pca$PC1, data_cluster_pca$PC2, col=k4$cluster, pch = 16)

#label the clusters
newdata <- data[1:80]
newdata$labels <- k4$cluster
table(newdata$Behavior,newdata$labels)
table(newdata$Treatment,newdata$labels)
table(newdata$Genotype,newdata$labels)


#######2.2 - Hierarchical Clustering
dd <- dist(data)
x11()
hc.complete =hclust(dist(data), method="complete")
hc.average =hclust(dist(data), method ="average") 
hc.single=hclust(dist(data), method ="single")

#complete method with Eucledian Distance
x11()
plot(hc.complete, 
     main="Complete Linkage with Eucledian Distance", xlab="", sub="", cex=.9)
abline (h=7.5, col =" red ")
plot(hc.average, 
     main="Average Linkage with Eucledian Distance", xlab="", sub="", cex=.9)
plot(hc.single, 
     main="Single Linkage with Eucledian Distance", xlab="", sub="", cex=.9)

#Determine the cluster labels for each observation associated with a given cut 
#of the dendrogram
cutree(hc.complete , 2)###


#Use correlation-based distance
data1 <- scale(data[,1:77])
ddc=as.dist(1- cor(t(data1)))
hcor.complete =hclust(ddc, method="complete")
hcor.average =hclust(ddc, method ="average") 
hcor.single=hclust(ddc, method ="single")

#plot Complete Linkage
x11()
hc_correlation <- plot(hcor.complete, main=" Complete Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=1.56, col =" red ")
hcor.complete.clusters <- cutree(hcor.complete,5)
hcor.complete.clusters

#plot Average Linkage
hc_correlation2 <- plot(hcor.average, main=" Average Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=1.56, col =" red ")

#plot Single Linkage
hc_correlation3 <- plot(hcor.single, main=" Single Linkage
with Correlation -Based Distance ", xlab="", sub ="")
abline (h=1.56, col =" red ")

#label the clusters
newdata <- data[1:80]
newdata$labels <- hcor.complete.clusters
table(newdata$Behavior,newdata$labels)
table(newdata$Treatment,newdata$labels)
table(newdata$Genotype,newdata$labels)

