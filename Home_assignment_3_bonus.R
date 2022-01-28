#### Radar plot
set.seed(5)
km.out <- kmeans(train , 5 , nstart =20)
###############################################################################
#### takt ehe center means
newdata <- as.data.frame(km.out$centers)
#### create personality traits
newdata$Popular <- (newdata$EXT1 + newdata$EXT3 + newdata$EXT5 +
                           newdata$EXT7 + newdata$EXT9)/5
newdata$Shy <- (newdata$EXT2 + newdata$EXT4 + newdata$EXT6 +
                           newdata$EXT8 + newdata$EXT10)/5
################################################################################
newdata$Inventive <- (newdata$OPN1 + newdata$OPN3 + newdata$OPN5 + newdata$OPN7 +
                            newdata$OPN8 +newdata$OPN9 +  newdata$OPN10)/7
newdata$Dull <- (newdata$OPN2 + newdata$OPN4 + newdata$OPN6)/3
################################################################################
newdata$Organized <- (newdata$CSN1 + newdata$CSN3 + newdata$CSN5 +
                            newdata$CSN7+ newdata$CSN9 + newdata$CSN10)/6
newdata$Messy <- (newdata$CSN2 + newdata$CSN4 + newdata$CSN6 +
                           newdata$CSN8)/4
################################################################################
newdata$Friendly <- (newdata$AGR2 + newdata$AGR4 + newdata$AGR6 +
                           newdata$AGR8 + newdata$AGR9 + newdata$AGR10)/6
newdata$Sociopath <- (newdata$AGR1 + newdata$AGR3 + newdata$AGR5 +
                              newdata$AGR7)/4
################################################################################
newdata$Stressed <- (newdata$EST1 + newdata$EST3 + newdata$EST5 +
                          newdata$EST6 + newdata$EST7+ newdata$EST8 +
                          newdata$EST9 + newdata$EST10)/8 
newdata$Relaxed <- (newdata$EST2 + newdata$EST4)/2
################################################################################
# To use the fmsb package, I have to add 2 lines to the dataframe: 
# the max and min of each topic to show on the plot!
data1 <- newdata[1,51:60]
data1 <- rbind(rep(5,10) , rep(1,10) , data1)
#################################################
data2 <- newdata[2,51:60]
data2 <- rbind(rep(5,10) , rep(1,10) , data2)
################################################
data3 <- newdata[3,51:60]
data3 <- rbind(rep(5,10) , rep(1,10) , data3)
#################################################
data4 <- newdata[4,51:60]
data4 <- rbind(rep(5,10) , rep(1,10) , data4)
#################################################
data5 <- newdata[5,51:60]
data5 <- rbind(rep(5,10) , rep(1,10) , data5)
#################################################
library(fmsb)
x11()
par(mfrow=c(2, 3))
radarchart(data1)
radarchart(data2)
radarchart(data3)
radarchart(data4)
radarchart(data5)