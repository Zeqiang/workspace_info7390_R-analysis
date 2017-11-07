
#-------------------------------------------------------------------------------------------------
# 1.
# EXPONENTIAL DISTRIBUTION
# USE CASE:
# The lifetime distribution of many electronic products generally follows the exponential distribution.
# The life distribution of some systems can also be approximated by an exponential distribution.

x <- seq(0,2,length.out=100)
y <- dexp(x, 2)
plot(x,y,col="blue",type ="l",main="The Exponential Density Distribution")
lines(x,dexp(x,1),col="green")
lines(x,dexp(x,5),col="orange")

#-------------------------------------------------------------------------------------------------
# 2.
# NORMAL DISTRIBUTION
# USE CASE:
# An average light bulb manufactured by a company lasts 23 days with a standard deviation of 10 days. 
# Assuming that bulb life is normally distributed, 
# what is the probability that an Acme light bulb will last at most 1 to 40 days			

Xaxis_NoOfDays <- seq (0,100, by=1)
Yaxis_Probablity <- dnorm(Xaxis_NoOfDays,51,10,FALSE)
plot( Xaxis_NoOfDays ,Yaxis_Probablity,col="blue",type ="l",main="The Normal Density Distribution")

#-------------------------------------------------------------------------------------------------
# 3.
# POISSON DISTRIBUTION
# USE CASE:
# Suppose you typically get 4 pieces of mail per day. That becomes your expectation. 
# But there will be a certain spread: sometimes a little more, sometimes a little less.
# Given only the average rate, for a certain period of observation, and assuming that the process, 
# or mix of processes, that produce the event flow are essentially random, 
# the Poisson Distribution will tell you how likely it is that you will get 3, or 5, or any other number, 
# during one period of observation.

x <- seq (1,100,by=1)
y <- dpois(x, 25, log = FALSE)
plot(x,y,col="blue",xlim=c(0,80),ylim=c(0,0.18),type ="l",main="The Poisson Density Distribution")
lines(x,dpois(x, 5, log = FALSE),col="green")
lines(x,dpois(x, 15, log = FALSE),col="orange")
lines(x,dpois(x, 35, log = FALSE),col="red")

#-------------------------------------------------------------------------------------------------
# 4.
# GAMMA DISTRIBUTION
# USE CASE:
# An important application of gamma distribution is that as a conjugate distribution 
# occurs in many machine learning algorithms. It assume that 
# the random variable X is the waiting time required to wait until the alpha event occurs.

x <- seq(0,10,length.out = 200)
y <- dgamma(x,3,2)
plot(x,y,col="blue",xlim=c(0,8),ylim=c(0,0.9),type ="l",main="The Gamma Density Distribution")
lines(x,dgamma(x,2,2),col="green")
lines(x,dgamma(x,5,1),col="orange")
lines(x,dgamma(x,9,1),col="red")

#-------------------------------------------------------------------------------------------------
# 5.
# BETA DISTRIBUTION
# USE CASE:
# Similiar with Gamma Distribution, Beta Distribution is a set of continuous probability 
# distributions defined in the (0,1), occuring in many machine learning algorithms.

x <- seq(0,10,length.out=10000)
y <- dgamma(x,15,4)
plot(x,y,col="blue",xlim=c(0,6),ylim=c(0,0.9),type ="l",main="The Beta Density Distribution")
lines(x,dgamma(x,15,3),col="green")
lines(x,dgamma(x,15,5),col="orange")
lines(x,dgamma(x,10,4),col="red")

#-------------------------------------------------------------------------------------------------
# 6.
# WEIBULL DISTRIBUTION
# USE CASE:
# The Weibull distribution is usually used in the field of failure analysis; 
# in particular, it can simulate the distribution of failure rate over time.
# For instance, Mechanical Manufacturing through the production line need to apply the reliability analysis.

x <- seq(0,2.5,length.out = 1000)
y <- dweibull(x,5)
plot(x,y,col="blue",xlim=c(0,2.5),ylim=c(0,6),type ="l",main="The WeiBull Density Distribution")
lines(x, dweibull(x, 1), type="l", col="red")
lines(x, dweibull(x, 1.5), type="l", col="green")
lines(x, dweibull(x, 15), type="l", col="purple")

#-------------------------------------------------------------------------------------------------
# 7.
# STUDENT'S T DISTRIBUTION
# USE CASE:
# This distribution is used to estimate the average number of female groups that are normally distributed.
# For example, when doctor excute vital sign examination for patients, it applies the 
# normally distribution, and need to the verified with T-distribution.

x <- seq(-5,5,length.out = 1000)
y <- dt(x,5,0)
plot(x,y,col="blue",xlim=c(-5,5),ylim=c(0,0.5),type ="l",main="The T Density Distribution")
lines(x,dt(x,1,0),col="green")
lines(x,dt(x,5,2),col="red")
lines(x,dt(x,50,4),col="orange")

#-------------------------------------------------------------------------------------------------
# 8.
# CHI-SQUARE DISTRIBUTION
# USE CASE:
# The chi-square is applied to the appropriate type of test. 
# According to a classification standard (variable), an event is divided into k class, 
# and it is checked whether the number distribution of each class in k class 
# conforms to the distribution of a certain theoretical number distribution.

x <- seq(0,10,length.out = 1000)
y <- dchisq(x,3)
plot(x,y,col="blue",xlim=c(0,8),ylim=c(0,1),type ="l",main="The Chi-Square Density Distribution")
lines(x,dchisq(x,1),col="green")
lines(x,dchisq(x,2),col="red")
lines(x,dchisq(x,8),col="orange")

#-------------------------------------------------------------------------------------------------
# 9.
# BINOMIAL DISTRIBUTION
# USE CASE:
# Binomial Distribution applies to the events that has two types of solution, true or false.
# For instance, the rate of hatching of certain batches of eggs is known to be 0.9, 20 eggs of the
# batch is extracted to be checked, and tt is found that the final 16 eggs hatch. 
# Then we are able to find the probability of binomial distribution.

x <- seq(1,220,length.out = 220)
y <- dbinom(x, size=500, prob=0.2)
plot(x,y,col="blue",xlim=c(0,220),ylim=c(0,0.06),type ="l",main="The Binomial Density Distribution")
lines(x,dbinom(x, size=500, prob=0.3),col="green")
lines(x,dbinom(x, size=800, prob=0.2),col="red")
lines(x,dbinom(x, size=500, prob=0.1),col="orange")

#-------------------------------------------------------------------------------------------------
# 10.
# UNIFORM DISTRIBUTION
# USE CASE:
# Randomly select 1000 students from the whole school, compare the rate between 2 genders

x <- seq(1,10,length.out = 10)
y <- table(sample(c(1:10), 100000, replace = TRUE, prob = NULL))
plot(x,y,col="blue",type ="p",main="The Uniform Density Distribution")

#-------------------------------------------------------------------------------------------------


