###########################################################
######### Please fill the ??? with proper description (atleast 130 charaters for each)
######### for SVM function try different values to achieve better results


# loading neccessary packages and dataset
# install.packages("caret")
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit

# write.csv(GermanCredit,"/Users/lzq/Documents/course material/INFO 7390 - Das/Assignments/week06/GermanCredit.csv",row.names = FALSE) 

# data standardlization 
# for the 1:7 column of dataset, the data is not scaled, use scale function to standardlize the data
# use as.data.frame() to make the standardlized data to be dataframe and replace the origanal dataset[,1:7]
str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)

# number of row s of dataset is 1000, randomly select 200 of 1000 rows to be the testing dataset
# and choose the rest of dataset(800 rows) as training dataset
sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
train_dateset = dataset[-sample_index,]

# use tune.svm function to calculate the svm model error & dispersion when the gamma = 10^(-6:-1), and cost = 10^(-1:1)
# summary the tune.svm model and get the best parameters for gamma & cost
# gamma = 0.01, cost = 1
# sampling method: 10-fold cross validation
tuned <- tune.svm(Class ~ ., data = train_dateset, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)

# setup the svm model using gamma = 0.01, cost = 1, kernel = "radial"
# using summary() function to get summary of svm model
model <- svm(Class ~ ., data = train_dateset, kernel = "radial", cost = 1, gamma = 0.01, scale = F)
summary(model)

# make the predictions using svm model with test_dateset, get the pred class for these rows
# input data is test_dateset[,-10], eliminate the 10th column
predictions <- predict(model, test_dateset[,-10])

# generate table of predictions value & actual value, compare the result
table(pred = predictions, true = test_dateset[,10])





