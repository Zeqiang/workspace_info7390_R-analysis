# exercise01------------------------------------------------------------------------------

#1. 
#(a) 
1:20 
#(b) 
20:1
#(c) 
c(1:20,19:1)
#(d) 
tmp <- c(4,6,3)
#(e) 
rep(tmp,10)
#(f) 
rep(tmp,l=31)
#(g) 
rep(tmp,times=c(10,20,30))
rep(tmp,each=10)

#2. 
tmp <- seq(3,6,by=0.1) 
exp(tmp)*cos(tmp)

#3. 
#(a)
(0.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3)) 
#(b) 
(2^(1:25))/(1:25)

#4. 
#(a)
tmp <- 10:100
sum(tmp^3+4*tmp^2)
#(b)
tmp <- 1:25
sum((2^tmp)/tmp + 3^tmp/(tmp^2))

#5. 
#(a) 
paste("label", 1:30)
#(b) 
paste("fn", 1:30, sep="")

#6.
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
#(a) 
yVec[-1] - xVec[-length(xVec)]
#(b) 
sin(yVec[-length(yVec)]) / cos(xVec[-1])
#(c) 
xVec[-c(249,250)] + 2*xVec[-c(1,250)]-xVec[-c(1,2)] 
#or, for an answer which works whatever the length of xVec,
xVecLen <- length(xVec)
xVec[-c(xVecLen-1,xVecLen)] + 2*xVec[-c(1,xVecLen)] - xVec[-c(1,2)] 
#(d) 
sum(exp(-xVec[-1])/(xVec[-length(xVec)]+10))

#7. 
#(a) 
yVec[yVec>600]
#(b) 
(1:length(yVec))[yVec>600]
#or 
which(yVec>600) 
#(c) 
xVec[yVec>600]
#(d) 
sqrt(abs(xVec-mean(xVec)))
#(e) 
sum( yVec>max(yVec)-200 )
#(f) 
sum(xVec%%2==0)
#(g) 
xVec[order(yVec)]
#(h) 
yVec[c(T,F,F)]

#8. 
1+sum(cumprod(seq(2,38,b=2)/seq(3,39,b=2)))



# exercise02------------------------------------------------------------------------------

#1.
#(a)
( tmp <- matrix( c(1,5,-2,1,2,-1,3,6,-3),nr=3) )
tmp%*%tmp%*%tmp
#The brackets round the first line ensure the matrix tmp is displayed 
#so that we can check that it has been entered correctly.
#(b) 
tmp[,3] <- tmp[,2]+tmp[,3]

#2.
tmp <- matrix(c(10,-10,10), b=T, nc=3, nr=15)
t(tmp)%*%tmp
#or 
crossprod(tmp)

#3.
matE <- matrix(0,nr=6,nc=6)
matE[ abs(col(matE)-row(matE))==1 ] <- 1

#4. 
outer(0:4,0:4,"+")

#5.
#(a) 
outer(0:4,0:4,"+")%%5
#or
matrix(0:4+rep(0:4,times=rep(5,5)),nc=5)
#(b) 
outer(0:9,0:9,"+")%%10
#(c) 
outer(0:8,0:8,"-")%%9

#6.
#Appropriate R code is
yVec <- c(7,-1,-3,5,17)
AMat <- matrix(0,nr=5, nc=5)
AMat <- abs(col(AMat)-row(AMat))+1
#To solve for x, calculate A−1y, by using the function solve to find the inverse of A.
#Either solve(AMat)%*%yVec which returns the values in x as a matrix with one column;
#or solve(AMat,yVec) which returns the values in x as a vector
#or solve(AMat,matrix(yVec,nc=1) ) which returns the values in x as a matrix with one column.
#If the result of any of these three expressions is saved as xVec, 
#then we can check the solution is correct by evaluating AMat%*%xVec 
#which returns the values in y as a matrix with one column in all three cases.

#7.
set.seed(75)
aMat <- matrix( sample(1:10, size=60, replace=T), nr=6)
#(a) 
apply(aMat, 1, function(x){sum(x>4)})
#(b) 
which( apply(aMat,1,function(x){sum(x==7)==2}) ) 
#(c) Here are two solutions:
aMatColSums <- colSums(aMat)
cbind( rep(1:10,rep(10,10)), rep(1:10,10) ) [outer(aMatColSums,aMatColSums,"+")>75,]
#or
aMatColSums <- colSums(aMat)
which( outer(aMatColSums,aMatColSums,"+")>75, arr.ind=T )
#If we wish to exclude repeats, we can use code such as
aMatColSums <- colSums(aMat)
logicalMat <- outer(aMatColSums,aMatColSums,"+")>75
logicalMat[lower.tri(logicalMat,diag=F)] <- F
which(logicalMat, arr.ind=T)

#8. 
#(a) 
sum( (1:20)^4 ) * sum( 1/(4:8) ) 
#or 
sum(outer((1:20)^4,4:8,"/"))
#(b) 
sum( (1:20)^4 / (3 + outer(1:20,1:5,"*")))
#(c) 
sum( outer(1:10,1:10,function(i,j){ (i>=j)*i^4/(3+i*j) }) )



# exercise03------------------------------------------------------------------------------

#1. 
#(a)
#xVec <- (1:n)
#seq(along=x)
tmpFn1 <- function(xVec)
{ 
  xVec^(1:length(xVec))
}
tmpFn2 <- function(xVec)
{ 
  n <- length(xVec) 
  (xVec^(1:n))/(1:n)
}
#(b)
tmpFn3 <- function(x, n)
{  
  1 + sum((x^(1:n))/(1:n)) 
}
#Always try out your functions on simple examples where you know the answer: 
#for example tmpFn1(1:3) should return the vector (1, 4, 27). 
#Also, check extreme cases: what happens if xVec has length 0? 
#Many functions require initial if statements which check that 
#the values of the function arguments satisfy the design requirements of the function—
#for example checking that the value of n is strictly positive in tmpFn3. 
#We have not included such code in our answers.

#2. 
tmpFn <- function(xVec) {
  n <- length(xVec)
  ( xVec[ -c(n-1,n) ] + xVec[ -c(1,n) ] + xVec[ -c(1,2) ] )/3
}
options(digits=0)
#or
tmpFn <- function(xVec)
{
  n <- length(xVec)
  ( x[1:(n-2)] + x[2:(n-1)] + x[3:n] )/3
}
options(digits=0)
#Note that tmpFn( c(1:5,6:1) ) should return the vector(2,3,4,5,5.333,5,4,3,2).

#3. 
tmpFn <- function(x) {
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3, 3, len=100)
plot(tmp, tmpFn(tmp), type="l")

#4. 
tmpFn <- function(mat) {
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  mat 
}

#5. 
#For the specific case of n=5 and k=2:
tmp <- diag(2, nr = 5)
tmp[abs(row(tmp) - col(tmp)) == 1] <- 1 
tmp
#Now for the function for the general case:
tmpFn <- function(n, k)
{
  tmp <- diag(k, nr = n)
  tmp[abs(row(tmp) - col(tmp)) == 1] <- 1
  tmp
}

#6. 
quadrant <- function(alpha) {
  1 + (alpha%%360)%/%90
}
#or
quadrant2 <- function(alpha)
{
  floor(alpha/90)%%4 + 1
}
#Both functions work on vectors, as any answer should!!

#7. 
weekday <- function(day, month, year) {
  month <- month - 2
  if(month <= 0) {
    month <- month + 12
    year <- year - 1
  }
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}
#The output of executing c( weekday(27,2,1997), weekday(18,2,1940), weekday(21,1,1963) )
#is the vector "Thursday", "Sunday", "Monday".
#Using if in the definition of weekday means that this function does not work on vectors. 
#However, we can eliminate the if statement as follows.
weekday2 <- function(day, month, year)
{
  flag <- month <= 2
  month <- month - 2 + 12*flag
  year <- year - flag
  cc <- year %/% 100
  year <- year %% 100
  tmp <- floor(2.6*month - 0.2) + day + year + year %/% 4 + cc %/% 4 - 2 * cc
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[1+tmp%%7]
}
#The output of executing weekday2( c(27,18,21), c(2,2,1), c(1997,1940,1963) ) 
#where all three input parameters are vectors is the vector "Thursday", "Sunday", "Monday".
#Clearly both weekday and weekday2 need extra lines of code which check that the values given for day,
#month and year are valid.

#8. 
#(a)
testLoop <- function(n)
{
  if(n<4)
    print("The argument n must be an integer which is at least 4.")
  else
  {
    xVec <- rep(NA, n-1)
    xVec[1] <- 1
    xVec[2] <- 2
    for( j in 3:(n-1) )
      xVec[j] <- xVec[j-1] + 2/xVec[j-1]
    xVec
  }
}
#Important. The colon operator has a higher precedence than the arithmetic operators 
#such as + or * but lower precedence than ^. 
#So always use brackets for constructs like 1:(n-1) or 1:(20^k) 
#so that the meaning is obvious even to those whose memory is faulty.
#Important. The above function gives the wrong answer if called with n=3. Why? 
#A line such as the following must be inserted:
#if( n <4 ) stop("The argument n must be an integer which is at least 4.\n")
#(b) 
#The following code is wrong. Why? 
testLoop2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(1:n) )
}
#The function testLoop2 returns the value of e0 + e1 if the vector yVec has length 0. 
#A correct function is 
testLoop2 <- function(yVec)
{
  n <- length(yVec)
  sum( exp(seq(along=yVec)) )
}
#This function now returns the correct value of 0 if yVec has length 0. 
#Important. Always use seq(along=x) rather than 1:length(x).

#9. 
#(a) 
#For a question like this where the value of xn must be known before the value of xn+1 can be calculated, 
#it is necessary to use a loop.
#First create the space for the answer with the code 
#xVec <- rep(NA, niter) 
#and then fill in the values. 
#Growing a vector inside a loop is very slow and inefficient. 
#Initialising the vector xVec to NA rather than to 0 makes it easier to spot certain errors: for example, 
#the error that the loop stops too early.
quadmap <- function(start, rho, niter)
{
  xVec <- rep(NA,niter)
  xVec[1] <- start
  for(i in 1:(niter-1)) {
    xVec[i + 1] <- rho * xVec[i] * (1 - xVec[i])
  }
  x 
}
tmp <- quadmap(start=0.95, rho=2.99, niter=500)
plot(tmp, type="l")
plot(tmp[300:500], type="l")
#(b)
quad2 <- function(start, rho, eps = 0.02)
{
  x1 <- start
  x2 <- rho*x1*(1 - x1)
  niter <- 1
  while(abs(x1 - x2) >= eps) {
    x1 <- x2
    x2 <- rho*x1*(1 - x1)
    niter <- niter + 1
  }
  niter
}

#10. 
#Values from the vector (x1 − x ̄, x2 − x ̄, . . . , xn − x ̄) are used three times in the expression for r
#k: twice in the numerator and once in the denominator.. 
#Therefore it is important to calculate this vector once and save its value for use in all three situations. 
#A function definition should not, for example, contain more than one occurrence of the expression mean(xVec). 
#Writing mean(xVec) more than once means that you are asking the programme to spend time calculating it more than once.
#(a)
tmpAcf0 <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}
#(b)
tmpAcf <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}




# exercise04------------------------------------------------------------------------------

#1. 
#(a)
fun4q1a <- function(xVec, yVec){
  colSums( outer(yVec, xVec, "<") )
}
#(b)
fun4q1b <- function(xVec, yVec){
  rowSums( sapply(yVec, FUN=function(y){y < xVec}) )
}
#(c)
fun4q1c <- function(xVec, yVec){
  rowSums( vapply(yVec, FUN=function(y){y<xVec}, FUN.VALUE=seq(along=xVec)) )
}
#And here is yet a fourth possible solution:
fun4q1d <- function(xVec,yVec)
{
  leny <- length(yVec)
  mat <- matrix(rep(xVec,leny), byrow=T, nrow=leny)
  apply( yVec<mat, 2, sum )
}
#(d) 
#Both fun4q1b and fun4q1d fail if either xVec or yVec has length 0; 
#but at least they do not give incorrect answers which would be far worse. 
#Both fun4q1a and fun4q1d fail if xVec and yVec are matrices.
#(e) 
#We can perform a timing by lines such as the following: 
rjr1 <- rnorm(10000)
rjr2 <- rnorm(12000) 
system.time(fun4q1a(rjr1,rjr2)) 
system.time(fun4q1b(rjr1,rjr2)) 
system.time(fun4q1c(rjr1,rjr2)) 
system.time(fun4q1d(rjr1,rjr2))
#The answer using vapply is the fastest.

#2. 
#(a)
# apply(is.na(matrix(c(1:3,NA,NA,4),nr=2)), 2, any)
tmpFn <- function(mat){
  mat[, !apply(is.na(mat), 2, any), drop = F]
}
#(b)
tmpFn2 <- function(mat){
  mat[!apply(is.na(mat), 1, any), !apply(is.na(mat), 2, any), drop = F]
}

#3. 
#(a) 
#First attempt:
empCopula <- function( u, v, xVec, yVec ) {
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  sum( (rVecN <= u) & (sVecN <= v) ) /n
}
#(b) 
#The answer to part (a) does not work if u and v are vectors, 
#but here are three solutions which do. 
#All three solutions are instructive.
#Suppose u = (u1,u2,...,uk) and v = (v1,v2,...,vk) and 
#set ri′ = ri/(n + 1) and s′i = si/(n + 1) for i = 1, 2, . . . , n.
#First solution using outer.
empCopula2 <- function( u, v, xVec, yVec ) {
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  valuesN <- colSums( outer(rVecN, u, "<=")&outer(sVecN, v, "<=") )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}
#In the above solution, outer(rVecN, u, "<=") gives the n × k logical matrix
#Hence the code outer(rVecN, u, "<=")&outer( sVecN, v, "<=") gives the n × k logical matrix
#and then we take the sums of the columns.
#Second solution using apply.
empCopula3 <- function( u, v, xVec, yVec ) {
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  tempf <- function(uv){
    sum( (rVecN <= uv[1]) * (sVecN <= uv[2]) )
  }
  valuesN <- apply( cbind(u,v), 1, tempf )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}
#In the above solution, the function tempf is applied to each row of the following k × 2 matrix:
#Third solution using mapply.
empCopula4 <- function( u, v, xVec, yVec ) {
  n <- length(xVec)
  rVecN <- rank(xVec)/(n+1)
  sVecN <- rank(yVec)/(n+1)
  valuesN <- mapply( FUN=function(u1,v1){ sum((rVecN<=u1)*(sVecN<=v1)) }, u, v )
  cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}
#The function mapply is a multivariate form of sapply. Thus
mapply( FUN=f, xVec, yVec )
#returns a vector of (f(x1, y1), f(x2, y2), . . . , f(xn, yn)) 
#where xVec is the vector (x1, x2, . . . , xn) and yVec is the vector (y1, y2, . . . , yn). 
#The recycling rule is applied if necessary. There can be any number of vectors.
#Our experience is that mapply is slow.
#The output from all three functions is a matrix and looks like this
#uCoord vCoord empCop
#[1,]  0.602  0.687  0.433
#[2,]  0.338  0.255  0.067
#[3,]  0.738  0.794  0.600

#4. 
#(a)
funA <- function (n)
{
  su <- 0
  for(r in 1:n)
  {
    for(s in 1:r)
      su <- su+s^2/(10+4*r^3)
  }
  su
}
#(b)
funB <- function (n)
{
  mat <- matrix(0, ncol=n, nrow=n)
  sum(  (col(mat)^2)/(10+4*row(mat)^3)*(col(mat)<=row(mat))  )
}
#(c)
funC <- function (n)
{
  sum( outer(1:n,1:n,FUN=function(r,s){ (s<=r)*(s^2)/(10+4*r^3) }) )
}
#(d)
funD <- function (n)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(sapply(1:n, FUN=tmpfn))
}
funE <- function (n)
{
  tmpfn <- function(r){sum(((1:r)^2)/(10+4*r^3))}
  sum(unlist(lapply(1:n, FUN=tmpfn)))
}
#(e)
funF <- function (n)
{
  tmpf <- function(s,r){(s^2)/(10+4*r^3)*(s<=r)}
  sum(mapply(tmpf, rep(1:n, times=rep(n,n)), 1:n))
}
#The fastest are funE and funD, but funB and funC are also quite fast. 
#The function funA is much slower and funF is even slower!

#5. 
#(a) 
#Here are two possible solutions
queue1 <- function(n, aRate, sRate)
{
  w <- 0
  for(i in 1:n){
    w <- max(0, w+rexp(1,sRate)-rexp(1,aRate))
  }
  w
}
queue2 <- function(n, aRate, sRate)
{
  w <- 0
  s <- rexp(n, sRate)
  a <- rexp(n, aRate)
  for(i in 1:n){
    w <- max(0, w+s[i]-a[i])
  }
  w
}
#Note that the second solution queue2 is considerably faster then the first.
#(b)
queueRep1 <- function (nReps, n, aRate, sRate)
{
  wVec <- rep(NA, nReps)
  for(j in 1:nReps)
    wVec[j] <- queue2(n, aRate, sRate)
  wVec
}
queueRep2 <- function (nReps, n, aRate, sRate)
{
  sapply( rep(n,nReps), queue2, aRate, sRate )
}
#or 
replicate(nReps, queue2(n,aRate,sRate)).
#(c)
queueRep3 <- function (nReps, n, aRate, sRate)
{
  w <- rep(0, nReps)
  s <- matrix(rexp(n*nReps, sRate), ncol=nReps)
  a <- matrix(rexp(n*nReps, aRate), ncol=nReps)
  for(i in 1:n){
    w <- pmax(0, w+s[i,]-a[i,])
  }
  w 
}
#There is very little difference between the times of queueRep1 and queueRep2. 
#However, the third method, 
#queueRep3, is considerably quicker then the other two methods—over 10 times quicker!

#6. 
#(a)
rwalk <- function(n)
{
  c( 0, cumsum(sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))) )
}
#(b) 
#This amount of time the walk spends above the x-axis is the same as 
#the number of points in the vector (s0 + s1, s1 + s2, . . . , sn−1 + sn) 
#which are greater than 0. Hence we get the function:
rwalkPos <- function(n)
{
  rw <- cumsum(c(0, sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5))))
  sum( (rw[-(n+1)] + rw[-1]) > 0 )
}
#(c)
rwalkPos1 <- function(nReps, n)
{
  results <- rep(NA, nReps)
  for(i in 1:nReps)
    results[i]<-rwalkPos(n)
  results 
}
rwalkPos2 <- function(nReps, n)
{
  replicate( nReps,  rwalkPos(n) )
}
#(d)
rwalkPos3 <- function(nReps, n)
{
  stepWalks <- matrix( sample( c(-1,1), n, replace=TRUE, prob=c(0.5,0.5)), nr=nReps )
  for(j in 2:n)
    stepWalks[,j] <- stepWalks[,j] + stepWalks[,j-1]
  stepWalks <- cbind(0, stepWalks)
  rowSums( stepWalks[,1:n] + stepWalks[,2:(n+1)]>0 )
}
#In this case, there is very little difference between the speed of the 3 functions 
#when nReps and n have small values—presumably because there is no vector form of cumsum 
#which has been replaced by an explicit loop in the function rwalkPos3 above. 
#For large values of nReps and n, the third version uses a lot of memory and will slow down 
#when paging occurs; 
#the replicate version uses less memory and so is generally preferable.




# exercise05------------------------------------------------------------------------------

#1. 
#(a)
tsEwma <- function( tsDat, m0=0, delta=0.7)
{
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=start(tsDat), frequency=frequency(tsDat))
}
#(b)
tsEwma2 <- function( tsDat, m0=0, delta=0.7)
{
  tsPars <- tsp(tsDat)
  tsDat <- c(tsDat)
  n <- length(tsDat)
  mVec <- rep(NA,n+1)
  mVec[1] <- m0
  for(j in 2:(n+1)){
    mVec[j] <- (1-delta)*tsDat[j-1] + delta*mVec[j-1]
  }
  ts(mVec[-1], start=tsPars[1], frequency=tsPars[3])
}
#For testing, we could try something like
tmp <- ts(rnorm(400000), start=c(1960,3), frequency=12)
system.time(tsEwma2(tmp))
system.time(tsEwma(tmp))
#On one of our computers, tsEwma2 took about one third the time of tsEwma.

#2. 
#(a)
myListFn <- function(n)
{
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}
#(b) 
#The line 
myList <- lapply( rep(10,4), myListFn ) 
#returns a list of 4 lists—one list for each call to myListFn. 
#The line 
myMatrix <- sapply( rep(10,4), myListFn ) 
#returns a 3 × 4 matrix—one row for xVec, one row for yVec and one row for count. 
#Thus myMatrix[1,1] is a vector of length 10 consisting of the 10 values in xVec 
#from the first call of myListFn.
#(c) 
#We first call 
myList <- lapply( rep(10,1000), myListFn )
#Here are three equivalent answers: 
lapply(myList, FUN=function(x){x[[2]]})
lapply(myList, FUN="[[", 2)
lapply(myList, FUN="[[", "yVec")
#(d) 
#Here are six equivalent answers:
sapply(myList, FUN="[[", 2)
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), 2)
sapply(myList, FUN=function(x){x[[2]]})
vapply(myList, FUN=function(x){x[[2]]}, FUN.VALUE=rep(0,10))
sapply(mList, FUN="[[", "yVec")
vapply(myList, FUN="[[", FUN.VALUE=rep(0,10), "yVec")
#(e)
myList2 <- lapply(myList, function(x){list(xVec=x$xVec, yVec=x$yVec)})
#(f) This code picks out the indices of those lists which satisfy the condition: 
which( unlist( lapply(myList, function(x){x[[3]]>2}) ) )
#So this is an answer:
myList[which(  unlist(lapply( myList, function(x){x[[3]]>2} ))  )]

#3. 
#(a)
partA <- sapply(myList, function(x){ sum(x$xVec*(1:10))/sum(x$yVec*(1:10)) })
#(b) 
#Here are 3 possible solutions:
myMat <- t(sapply( myList, function(x){x$xVec-x$yVec})) 
myMat2 <- matrix( unlist( lapply(myList, FUN="[[",1) ) - 
                    unlist( lapply(myList, FUN="[[",2) ), nc=10, by=T  )
myMat3 <- matrix(  unlist(lapply(myList, function(x){x$xVec-x$yVec})), nc=10, by=T  )
#(c) 
#Here is a quick solution using sapply:
sum(sapply(myList, function(x){x$xVec[2]})*(1:1000)) /
  sum(sapply(myList, function(x){x$yVec[2]})*sapply(myList, function(x){x$count}))
#An alternative solution uses the fact that if a list has components with equal lengths 
#then it can be converted into a data.frame—although this operation is slow. 
#Hence data.frame(myList) is a 10 × 3000 data.frame. 
#Columns 1, 4, 7, etc are for the vectors called xVec; 
#columns 2, 5, 8, etc are for the vectors called yVec and 
#columns 3, 6, 9, etc are for the values of count. 
#Recycling is used—-hence every entry in column 3 is equal to n1, etc.
#Hence the following two lines
myDf <- data.frame(myList)
myDf[2,  seq(1,3000,by=3)]
#pick out the vector (x12, x22, . . . , x1000,2).
#Calculations are faster on matrices than data frames, so we proceed as follows:
myMat <- as.matrix(data.frame(myList))
names(myMat) <- NULL
sum((1:1000) * myMat[2,seq(1,3000,by=3)])/
  sum(myMat[2,  seq(3,3000,by=3)] * myMat[2,  seq(2,3000,by=3)])
#The last line could be replaced by
sum( (1:1000) * myMat[2,c(T,F,F)] )/sum( myMat[2,c(F,F,T)] * myMat[2,c(F,T,F)] )
#The intermediate step of converting to a matrix only gives a worthwhile reduction in time 
#if a lot of such calculations are to be made—otherwise it is not sensible to include it.

#4.
#(a) 
#The code 
apply(testArray, c(2,3), min) 
#returns a d2 × d3 matrix with entries wj,k where ----
#Hence the code
sweep(testArray, c(2,3), apply(testArray, c(2,3), min))
#returns a d1 × d2 × d3 matrix with entries wi,j,k where d1 ----
#For the matrix with entries {zj,k} we just need
apply(testArray, c(2,3), sum) - apply(testArray, c(2,3), max)
#or, better
apply(testArray, c(2,3), FUN=function(x){ sum(x) - max(x)})
#So our function is
testFn2 <- function(xArray)
{
  wArray <- sweep(testArray, c(2,3), apply(testArray, c(2,3), min))
  zArray <- apply(testArray, c(2,3), FUN=function(x){ sum(x) - max(x)})
  list(wArray=wArray, zArray=zArray)
}
#(b) 
#Now the code
tmp <- apply(testArray, c(1,2), FUN=function(x){ x^(1:length(x))})
#returns the d3 × d1 × d2 array with entries zm,n,r = xmn,r,m.
#We now need to sum over the second coordinate of the {zm,n,r} and 
#return the other two coordinates in the order (r, m) which is done by the following code:
apply(tmp, c(3,1), sum)
#Hence our function is
testFn <- function( xArray)
{
  apply( apply(xArray, c(1,2), FUN=function(x){x^(1:length(x))}), c(3,1), sum )
}

#5. 
#(a)
shift <- function(X,a,b){
  X[,1] <- X[,1] + a
  (b)
  X[,2] <- X[,2] + b
  X
}
#(b)
rotate <- function(X,r){
  X%*%matrix(c(cos(r), -sin(r), sin(r), cos(r)), nrow = 2)
}
#To try shift and rotate on matrix A create it via
A <- cbind(c(0,1,2,4/9,14/9), c(0,3,0,4/3,4/3)) 
#We also use A thus created in the code that follows.
#(c) 
#If your solution looks something like 
arrayA <- array(0, dim=c(5,2,25))
for(i in 1:25){
  arrayA[,,i] <- rotate(A,2*pi*(i-1)/24)
}
#then look up function vapply and try to eliminate the loop from the code above. 
#Here is an alternative: 
arrayA<-vapply(1:25,
               FUN=function(i){
                 rotate(A,2*pi*(i-1)/24)
               },
               matrix(0,nrow=5, ncol=2)
)
#(1)
plot(c(-10,10), c(-10,10), ann=F, type='n')
for(i in 1:25)
  drawA(arrayA[,,i])
#or
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
#Note that the function invisible suppresses display of the output of sapply, 
#since the output is NULL and we are only interested in the resulting plot.
#(2)
plot(arrayA[2,1,], arrayA[2,2,])
#(3)
plot(1:25, arrayA[2,1,])
#(d)
scale <- function(X,a,b){
  X%*%matrix(c(a,0,0,b), nrow=2)
}
arAscaled <- vapply(1:25,
                    FUN=function(i){
                      scale(arrayA[,,i],2,3)
                    },
                    matrix(0,nrow=5, ncol=2)
)
plot(c(-10,10), c(-10,10), ann=F, type='n')
invisible(sapply( 1:25, FUN=function(i){ drawA(arrayA[,,i]) } ))
invisible(sapply( 1:25, FUN=function(i){ drawA(arAscaled[,,i]) } ))
#(e) 
#First, as before, create an empty array randomA of appropriate size and initiate layer 1 to A. 
arArandom <- array(0, dim=c(5,2,25))
arArandom[,,1] <- A
#Now, since for i = 2, ..., 25 each A[,,i] should depend on A[,,i-1] in a random manner, 
#we cannot use vapply, but have to create a loop instead:
for(i in 2:25){
  arArandom[,,i] <-
    shift(
      rotate(
        scale(arArandom[,,i-1], runif(1,0.5,1.5),runif(1,0.5,1.5)),
        2*pi*runif(1,-1,1)
      ),
      runif(1,-1,1), runif(1,-1,1)
    )
}
#Then create an animation:
oopt = ani.options(interval = 0.2, nmax = 25)
for (i in 1:ani.options("nmax")){
  plot(c(-10,10), c(-10,10), ann=F, type='n')
  drawA(arArandom[,,i])
  ani.pause()
}
#Limit values for scaling, rotation and shifting are arbitrary and you are encouraged 
#to play around with them to see how they affect dynamics of ‘A’. 
#(Make sure you create a large enough initial empty graph though!)







