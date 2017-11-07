# ----------------------------------------------------------------------------------------------------------------------------------------------
# Assignment 04_p2

# install.packages("MASS")
# install.packages("car")
# install.packages("ridge")
# install.packages("glmnet")

# ----------------------------------------------------------------------------------------------------------------------------------------------
# 1. Regularization: Ridge Regression

# get origional data
set.seed(10)
X <- seq(60,300,by=4)*pi/180
Y <- sin(X) + rnorm(length(X),mean=0, sd=0.15)
originalData <- data.frame(X,Y)
names(originalData) <- c("x", "y")
plot(X, Y)

# add radom x
X1 <- X^2
X2 <- X^3
X3 <- X^4
X4 <- X^5
X5 <- X^6
X6 <- X^7
# X1to8 <- outer(X,1:7,"^")
Xmat <- data.frame(X, X1, X2, X3, X4, X5, X6)
originalData <- data.frame(Y, Xmat)
names(originalData) <- c("y", "x", "x1", "x2", "x3", "x4", "x5", "x6")
names(Xmat) <- c("x", "x1", "x2", "x3", "x4", "x5", "x6")

# Linear regression
data.fit <- lm(y ~ ., data = originalData)
summary(data.fit)

# check Collinear for different X
library("car")
vif(data.fit)

# Ridge regression
library(MASS)
data.ridge <- lm.ridge(y  ~ ., lambda = 10^seq(2, -2, by = -0.1), data = originalData, model = TRUE)
summary(data.ridge)

# find the lambdaGCV & Cofficients for least GCV
bestLambda <- data.ridge$lambda[which.min(data.ridge$GCV)]
data.ridge$coef[which.min(data.ridge$GCV)]
matplot(data.ridge$lambda, t(data.ridge$coef), xlab = expression(lamdba), ylab = "Cofficients", type = "l", lty = 1:20)

# Using glmnet to set up Ridge model
library(glmnet)
X_process <-as.matrix(Xmat)
Y_process <- as.double(as.matrix(Y))
ridge.model <- cv.glmnet(X_process,Y_process,alpha=0,lambda = 10^seq(2, -2, by = -0.1))

# get glmnet.fit to get Y_predict
ridge.fit <- ridge.model$glmnet.fit
plot(fit)
Y_predict <- predict(ridge.fit, s = bestLambda, newx = X_process)

# make the prediction lot
plot(X,Y_predict,type = "l")
lines(X,Y,type = "p",xlab = "XValue",ylab = "YValue")

# ----------------------------------------------------------------------------------------------------------------------------------------------
# 2. Regularization: Lasso Regression

# Using glmnet to set up Lasso model
library(glmnet)
data.lasso <- cv.glmnet(X_process,Y_process,alpha = 1)
plot(data.lasso)

# get glmnet.fit to get Y_predict
data.glmnet.fit <- data.lasso$glmnet.fit
Y_predict_lasso <- predict(data.glmnet.fit, s = bestLambda, newx=X_process)

# make the prediction lot
plot(X,Y_predict_lasso,type = "l")
lines(X,Y,type = "p",xlab = "XValue",ylab = "YValue")

# ----------------------------------------------------------------------------------------------------------------------------------------------






