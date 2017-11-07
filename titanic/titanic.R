# Import the training set: train
train <- read.csv("C:\\Users\\kein\\Desktop\\data science\\week3\\titanic train.csv")
# Import the testing set: test
test <- read.csv("C:\\Users\\kein\\Desktop\\data science\\week3\\titanic test.csv")

# Install and load required packages for fancy decision tree plotting
#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#The last passanger had missing information in all fields, except for the age variable, therefore was excluded from the dataset.
tail(train)
train1 <- train[-1310,]

ggplot(train1,aes(x=factor(Pclass),fill=factor(Sex)))+
  geom_bar(position="dodge")
require(ggplot2)
ggplot(train1,aes(x=factor(Pclass),fill=factor(Sex)))+
  geom_bar(position="dodge")+
  facet_grid(". ~ survived")

# Check the survival rates in absolute numbers
table(train$Survived)

# compare the survival rate between different gender
table(train$Sex, train$Survived)

# create the gender model first
model <- rpart(Survived ~ Sex, data=train, method="class")
rpart.plot(model)

# Build a decision tree
baseModel <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# then draw a decision tree
rpart.plot(baseModel)

# make a prediction 
Prediction <- predict(baseModel, test, type = "class")


# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId,
                     Pclass =test$Pclass
                     , Sex =test$Sex
                     ,Age =test$Age
                     ,SibSp =test$SibSp
                     ,Parch =test$Parch
                     ,Fare =test$Fare
                     ,Embarked =test$Embarked
                     ,Survived = Prediction)

#write a result file
write.csv(submit, file = "C:\\Users\\kein\\Desktop\\data science\\week3\\myPrediction.csv", row.names = FALSE)

#show the result
nrow(my_solution)

my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                       data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
rpart.plot(my_tree)


########################################################################################



