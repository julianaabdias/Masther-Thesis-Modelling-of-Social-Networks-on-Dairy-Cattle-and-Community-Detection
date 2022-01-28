#Home Assignment

# Question 1: Recode the variable Trump as follows. 
#Denote Slightly liberal to Extremely liberal (levels 1-3) as "Liberal", 
#and Moderate to Extremely conservative (levels 4-7) as "Conservative". 

#cleaning the environment
rm(list=ls())
library(ISLR)

#setting the working directory to where the csv file is saved
getwd()
setwd("C:/Users/julia/Documents/Master BI/Statistical Learning/Home Assignment")

#Import csv file into R
dataframe <- read.csv(file='ANES2016.csv')

#Read the first 6 elements of each variable and the summary
head(dataframe)
summary(dataframe)

#View the whole dataframe
dim(dataframe)
View(dataframe)

#recoding variables Trump Slightly liberal to Extremely liberal == Liberal
#and dummy coding all Liberal as 1
dataframe[dataframe$Trump == 1, "Trump"] <- 1
dataframe[dataframe$Trump == 3, "Trump"] <- 1
dataframe[dataframe$Trump == 2, "Trump"] <- 1

#recoding variables Trump Moderate to Extremely conservative == Conservative
#and dummy coding all Conservative as 0
dataframe[dataframe$Trump == 4, "Trump"] <- 0
dataframe[dataframe$Trump == 5, "Trump"] <- 0
dataframe[dataframe$Trump == 6, "Trump"] <- 0
dataframe[dataframe$Trump == 7, "Trump"] <- 0


##now I'm creating a subset of Trump variable with only 2 responses

#attaching the dataframe in order to access the variables directly
attach(dataframe)

#checking if there is any NA value in the column Trump
sum(is.na(Trump))

#taking dimension of rows in the whole dataframe in order to compare with the subset
rows_dataframe <- nrow(dataframe)
rows_dataframe

#creating the subset where the variable Trump = Liberal or Conservative
subset <- subset(dataframe, Trump == 1 | Trump == 0, select = 1:18)

#checking if the subset is correct
View(subset)

#double checking if the subset contains all rows it should (except Refused and Don't Know)
sum_others <- sum(dataframe$Trump == -9 | dataframe$Trump == -8)
rows_subset <- nrow(subset)
rows_subset
rows_subset == (rows_dataframe - sum_others)

#excluding NA values from the subset
sum(is.na(subset))
subset2 <- na.omit(subset)


##now the subset is ready

#Question 1: 
#Is there any personal characteristics of the individuals that determines whether 
#someone would consider Donald Trump as Liberal (or conservative)? 
#Motivate your methods and interpret your results.

#fitting the model (using multiple logistic regression because we have a binary
#response using multiple predictors)

#this model predict the relationship with variables and Trump == Liberal, coded as 1

#take a look at the number of respondents whether they think Trump is liberal or conservative
install.packages("ggplot2")
library(ggplot2)
ggplot(subset2, aes(Trump))+
  geom_bar(fill = 'blue')


### glm with all variables except ID, which doesn't give any information about personal 
#characteristcs
attach(subset2)

glm.fit <- glm(Trump ~ . - ID, data = subset2, family = binomial)
summary(glm.fit)

#statistically significant variables :hillary, spoused, dependent, income, education2

#now I fit the prediction using this model
prob <- predict(glm.fit, type = "response")

#creating a vector with of the elements for which the predicted probability of Trump
#being considered Liberal exceeds 50% 
pred <- ifelse(prob > 0.5, "1", "0")

#confusion matrix to check how many observations were correctly or incorrectly classified
xtabs(~Trump+pred,data=subset2)

#same matrix but as proportion matrix instead
prop.table(xtabs(~Trump+pred,data=subset2),1)

#probability of predictions being correct
mean(pred == Trump)

#training error rate
mean(pred != Trump)

###trying backward selection here to see if the results change
#selecting best size model by using backwards selection
library(leaps)
dim(subset2)
regfit.bwd = regsubsets(Trump ~ .-ID, data = subset2, nvmax = 17 , method = "backward")
summary(regfit.bwd)
reg.summary <- summary(regfit.bwd)

#comparing the size of models using cp, bic and adjr2
which.min(reg.summary$cp)
which.min(reg.summary$bic)
which.max(reg.summary$adjr2)

#checking the variables selected in the backward selection method using Cp (AIC)
coeffs <- coef(regfit.bwd, id = 9)
names(coeffs)

#coeffs: Media, Hilary, Age, SpouseEdu, GBirth, Dependent, Housing, Income, Education2

#fitting glm with only the variables selected by backward method
glm.fit2 <- glm(Trump ~ Media + Hilary + Age + SpouseEdu + GBirth + Dependent+
                  Housing + Income + Education2, data = subset2, family = binomial)
summary(glm.fit2)

  #statistically relevant variables: Hilary, SpouseEdu, Dependent, Income, Education2

#now I fit the prediction using this model
prob2 <- predict(glm.fit2, type = "response")

#creating a vector with of the elements for which the predicted probability of Trump
#being considered Liberal exceeds 50% 
pred2 <- ifelse(prob2 > 0.5, "1", "0")

#confusion matrix to check how many observations were correctly or incorrectly classified
xtabs(~Trump+pred2,data=subset2)

#same matrix but as proportion matrix instead
prop.table(xtabs(~Trump+pred2,data=subset2),1)

#mean of correct prediction
mean(pred2 == Trump)

#training error rate
mean(pred2!=Trump)


###divide the data into training and test set and check if the accuracy changes, if
#the error rate change

#divide the data into train data (2/3) and test data (1/3)
set.seed(27)
train <- sample(1:nrow(subset2), size=2694, replace=F) 

# glm with all variables except ID, which doesn't give any information about personal characteristcs
#using training data
glm.fit3 <- glm(Trump ~ . -ID, data = subset2[train,], family = binomial)
summary(glm.fit3) 
#statistically relevant variables: Hilary, Education, GBirth, Dependent, Income

#now I fit the prediction using this model
prob3 <- predict(glm.fit3, newdata = subset2[-train,], type = "response")

#creating a vector with of the elements for which the predicted probability of Trump
#being considered Liberal exceeds 50% 
pred3 <- ifelse(prob3 > 0.5, "1", "0")

#confusion matrix to check how many observations were correctly or incorrectly classified
xtabs(~pred3 + subset2$Trump[-train])

#same matrix but as proportion matrix instead
prop.table(xtabs(~pred3 + subset2$Trump[-train]),1)

##probaliby of correct prediction
mean(pred3 == subset$Trump[-train])

#test error
mean(pred3 != subset$Trump[-train])

##Cross-validation of the model using all variables except ID, using train and test data
#Error 1
err <- numeric(3)
err [1] <- mean(pred3 != subset$Trump[-train])

#Error 2
train <- sample(1:nrow(subset2), size=2694, replace=F) 
glm.fit4 <- glm(Trump ~ . -ID, data = subset2[train,], family = binomial)
summary(glm.fit4) 
prob4 <- predict(glm.fit4, newdata = subset2[-train,], type = "response")
pred4<- ifelse(prob4 > 0.5, "1", "0")
xtabs(~pred4 + subset2$Trump[-train])
prop.table(xtabs(~pred4 + subset2$Trump[-train]),1)
err [2] <- mean(pred4 != subset$Trump[-train])

#Error 3
train <- sample(1:nrow(subset2), size=2694, replace=F) 
glm.fit5 <- glm(Trump ~ . -ID, data = subset2[train,], family = binomial)
summary(glm.fit5) 
prob5 <- predict(glm.fit5, newdata = subset2[-train,], type = "response")
pred5<- ifelse(prob5 > 0.5, "1", "0")
xtabs(~pred5 + subset2$Trump[-train])
prop.table(xtabs(~pred5 + subset2$Trump[-train]),1)
err [3] <- mean(pred5 != subset$Trump[-train])
err


#Question 2
#Build a suitable prediction model to predict an individual's party identification 
#using the respective individual's other personal, and family characteristics. 
#Experiment with different methods, and model specifications, and motivate your choice.

summary(subset2)
View(subset2$PartyID)
sum(is.na(PartyID))


#it's classification prediction problem with 4 variables outcome and multiple predictors
#using LDA for logistic regression with multiple-class classification

#first loading MASS library to be able to execute lda function
library(MASS)

#fitting lda with PartyID as outcome variable and all others personal characteristics, except ID
#using train data
train <- sample(1:nrow(subset2), size=2694, replace=F)
boxplot(PartyID)
lda_fit <- lda(PartyID ~ . -ID, data = subset2[train,])  
lda_fit  
plot(lda_fit)

#prediction using test data
lda.pred = predict(lda_fit,newdata = subset2[-train,])
class(lda.pred)
names(lda.pred)

#check the first 6 predictions and it's posterior probabilities
data.frame(lda.pred)[1:6,]

#confusion matrix
table(lda.pred$class,subset2[-train,]$PartyID)
prop.table(xtabs(~lda.pred$class + subset2$PartyID[-train]),1)

#probability of prediction being correct
mean(lda.pred$class== subset2[-train,]$PartyID)

#applying a threshold to the posterior probabilities of 50%
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)


#Trying other methods : QDA

#fitting QDA with PartyID as outcome variable and all others personal characteristics, except ID
#using train data
qda.fit <- qda(PartyID ~ . -ID,data=subset2[train,])
qda.fit

#prediction using test data
qda.class = predict(qda.fit,newdata = subset2[-train,])$class

#confusion matrix
table(qda.class, subset2[-train,]$PartyID)
prop.table(xtabs(~qda.class + subset2$PartyID[-train]),1)

#probability of prediction being correct
mean(qda.class == subset2[train,]$PartyID)

##Trying other methods : KNN
dim(subset2)
View(subset2)
library(class)
x_train <-cbind(subset2[train,]$Media, subset2[train,]$FamSize, subset2[train,]$Hilary,
                subset2[train,]$Age, subset2[train,]$Partner, subset2[train,]$Education,
                subset2[train,]$SpouseEdu, subset2[train,]$Employment,subset2[train,]$Birthplace, 
                subset2[train,]$GBirth, subset2[train,]$Dependent, subset2[train,]$Housing, 
                subset2[train,]$Income, subset2[train,]$Education2, subset2[train,]$PartyID, 
                subset2[train,]$Marital)
x_test <- cbind(subset2[-train,]$Media, subset2[-train,]$FamSize, subset2[-train,]$Hilary,
                subset2[-train,]$Age, subset2[-train,]$Partner, subset2[-train,]$Education,
                subset2[-train,]$SpouseEdu, subset2[-train,]$Employment,subset2[-train,]$Birthplace, 
                subset2[-train,]$GBirth, subset2[-train,]$Dependent, subset2[-train,]$Housing, 
                subset2[-train,]$Income, subset2[-train,]$Education2, subset2[-train,]$PartyID, 
                subset2[-train,]$Marital)

knn.pred1 = knn(train=x_train,test = x_test,cl = subset2[train,]$PartyID,k=1)
prop.table(xtabs(~subset2[-train,]$PartyID+knn.pred1),1)
mean(knn.pred1 == subset2[-train,]$PartyID)


##Trying other methods : if KNN = 2

x_train2 <-cbind(subset2[train,]$Media, subset2[train,]$FamSize, subset2[train,]$Hilary,
                subset2[train,]$Age, subset2[train,]$Partner, subset2[train,]$Education,
                subset2[train,]$SpouseEdu, subset2[train,]$Employment,subset2[train,]$Birthplace, 
                subset2[train,]$GBirth, subset2[train,]$Dependent, subset2[train,]$Housing, 
                subset2[train,]$Income, subset2[train,]$Education2, subset2[train,]$PartyID, 
                subset2[train,]$Marital)
x_test2 <- cbind(subset2[-train,]$Media, subset2[-train,]$FamSize, subset2[-train,]$Hilary,
                subset2[-train,]$Age, subset2[-train,]$Partner, subset2[-train,]$Education,
                subset2[-train,]$SpouseEdu, subset2[-train,]$Employment,subset2[-train,]$Birthplace, 
                subset2[-train,]$GBirth, subset2[-train,]$Dependent, subset2[-train,]$Housing, 
                subset2[-train,]$Income, subset2[-train,]$Education2, subset2[-train,]$PartyID, 
                subset2[-train,]$Marital)

knn.pred2 = knn(train=x_train2,test = x_test2,cl = subset2[train,]$PartyID,k=2)
prop.table(xtabs(~subset2[-train,]$PartyID+knn.pred2),1)
mean(knn.pred2 == subset2[-train,]$PartyID)


###comparing using a different division in training and data set: 80% training and 20% test
dim(subset2)
4041*0.8
train2 <- sample(1:nrow(subset2), size=3233, replace=F)

#fit LDA again with this new train and test set
lda_fit2 <- lda(PartyID ~ . -ID, data = subset2[train2,])  
lda_fit2  
#prediction using test data
lda.pred2 = predict(lda_fit2,newdata = subset2[-train2,])
class(lda.pred2)
names(lda.pred2)

#confusion matrix
table(lda.pred2$class,subset2[-train2,]$PartyID)
prop.table(xtabs(~lda.pred2$class + subset2$PartyID[-train2]),1)

#probability of prediction being correct
mean(lda.pred2$class== subset2[-train2,]$PartyID)

##QDA
#fitting QDA with new proportion of training and test data
qda.fit2 <- qda(PartyID ~ . -ID,data=subset2[train2,])
qda.fit2

#prediction using test data
qda.class2 = predict(qda.fit2,newdata = subset2[-train2,])$class

#confusion matrix
table(qda.class2, subset2[-train2,]$PartyID)
prop.table(xtabs(~qda.class2 + subset2$PartyID[-train2]),1)

#probability of prediction being correct
mean(qda.class2 == subset2[train2,]$PartyID)


##fitting KNN = 1
x_train3 <-cbind(subset2[train2,]$Media, subset2[train2,]$FamSize, subset2[train2,]$Hilary,
                 subset2[train2,]$Age, subset2[train2,]$Partner, subset2[train2,]$Education,
                 subset2[train2,]$SpouseEdu, subset2[train2,]$Employment,subset2[train2,]$Birthplace, 
                 subset2[train2,]$GBirth, subset2[train2,]$Dependent, subset2[train2,]$Housing, 
                 subset2[train2,]$Income, subset2[train2,]$Education2, subset2[train2,]$PartyID, 
                 subset2[train2,]$Marital)
x_test3 <- cbind(subset2[-train2,]$Media, subset2[-train2,]$FamSize, subset2[-train2,]$Hilary,
                 subset2[-train2,]$Age, subset2[-train2,]$Partner, subset2[-train2,]$Education,
                 subset2[-train2,]$SpouseEdu, subset2[-train2,]$Employment,subset2[-train2,]$Birthplace, 
                 subset2[-train2,]$GBirth, subset2[-train2,]$Dependent, subset2[-train2,]$Housing, 
                 subset2[-train2,]$Income, subset2[-train2,]$Education2, subset2[-train2,]$PartyID, 
                 subset2[-train2,]$Marital)

knn.pred3 = knn(train=x_train3,test = x_test3,cl = subset2[train2,]$PartyID,k=1)
prop.table(xtabs(~subset2[-train2,]$PartyID+knn.pred3),1)
mean(knn.pred3 == subset2[-train2,]$PartyID)
