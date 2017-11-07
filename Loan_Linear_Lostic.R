# ----------------------------------------------------------------------------------------------------------------------------------------------
# Assignment 02

library(XLConnect)
library(stats)
# ----------------------------------------------------------------------------------------------------------------------------------------------

# 1. Linear regression:
# http://reliawiki.org/index.php/Multiple_Linear_Regression_Analysis
# Multiple Linear Regression Analysis

# Calculate F0
originalData <- readWorksheetFromFile("/Users/lzq/Documents/course material/INFO 7390 - Das/Assignments/week02/Submit/source data.xlsx", sheet = 1)
xMat <- matrix(c(rep(1,17), originalData$Factor1, originalData$Factor2), ncol = 3)
yMat <- matrix(originalData$Yield, ncol = 1)

beta <- solve(t(xMat)%*%xMat)%*%(t(xMat)%*%yMat)
H <- xMat %*% solve((t(xMat) %*% xMat)) %*% t(xMat)
J <- matrix(rep(rep(1, 17), 17), ncol = 17, nrow = 17)
SSr <- t(yMat) %*% (H - (J/17)) %*% yMat
MSr <- SSr/2
SSe <- t(yMat) %*% (diag(17) - H) %*% yMat
MSe <- SSe/(17-(2+1))
F0 <- MSr/MSe
F0

# Linear regression summary
originalData.fit <- lm(originalData$Yield ~ originalData$Factor1 + originalData$Factor2)
summary(originalData.fit)
plot(originalData.fit)


# --------------------------------------------------------------------------------------------------------------------------------------------------------------

# 2. Logistics regerssion
# Run logistics regression on the loan example with the variable Decision as the dependent variable and the five categorical variables identified 
# in the class (Res_status, Occupation, Job_status, Liab_ref, Acc_ref) as the independent variables. Show your prediction for 
# input (owner, creative_, governmen, f, given) and (rent, creative_, governmen, f, given).

LoanData <- readWorksheetFromFile("/Users/lzq/Documents/course material/INFO 7390 - Das/Assignments/week02/Submit/loan.xlsx", sheet = 1)

LoanData['Decision'][LoanData['Decision']=='accept']<-1
LoanData['Decision'][LoanData['Decision']=='reject']<-0
LoanData$Decision <- as.numeric(LoanData$Decision)
loanModel <- glm (Decision ~ Res_status + Occupation + Job_status + Liab_ref + Acc_ref, data = LoanData, family = binomial())
summary(loanModel)

# Prediction
input_1 <- data.frame(Res_status = "owner", Occupation = "creative_", Job_status = "governmen", Liab_ref = "f", Acc_ref = "given")
predict_1 <- predict(loanModel, input_1, type  = "response")

input_2 <- data.frame(Res_status = "rent", Occupation = "creative_", Job_status = "governmen", Liab_ref = "f", Acc_ref = "given")
predict_2 <- predict(loanModel, input_2, type = "response")

# --------------------------------------------------------------------------------------------------------------------------------------------------------------




