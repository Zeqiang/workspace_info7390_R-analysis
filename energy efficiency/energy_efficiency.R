library(rpart)
Energy_efficiency <- read.csv("C:\\Users\\kein\\Desktop\\data science\\week3\\Energy Efficiency ENB2012_data.csv", header = TRUE, sep = ",")
head(Energy_efficiency)
data_y1 <- rpart(Y1~X1 + X2 + X3 + X4 +X5 + X6 +X7 +X8, method="anova", data=Energy_efficiency)
data_y2 <- rpart(Y2~X1 + X2 + X3 + X4 +X5 + X6 +X7 +X8, method="anova", data=Energy_efficiency)


head(data_y1)#Check the dataset
names(data_y1)# Check the column names
attach(data_y1) # attach dataset

head(data_y2)#Check the dataset
names(data_y2)# Check the column names
attach(data_y2) # attach dataset


printcp(data_y1)# tree regression
plotcp(data_y1) 
printcp(data_y2) 
plotcp(data_y2)

library(rattle)
library(rpart.plot)
library(RColorBrewer)
prp(data_y2)
rpart.plot(data_y1)
rpart.plot(data_y2)
