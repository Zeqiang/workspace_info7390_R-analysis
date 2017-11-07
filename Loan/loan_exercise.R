# 4. 
#Read the loan data excel file (uploaded in the course material section) into a frame 
#and then export only those rows with age greater than 30 
#and unemployed to another sheet of the same excel file.

#install.packages("xlsx")
#library(xlsx)
library(XLConnect)
wk = loadWorkbook(
  "/Users/lzq/Documents/course material/INFO 7390 - Das/Assignments/Assignment01/loan & Dis/loan.xlsx") 

data <- readWorksheet(wk,sheet = "loan")

newloan <- subset(data, data$Age > 30 & data$Job_status=="unemploye")

createSheet(wk,name="Loan_FilteredData")
writeWorksheet(wk,newloan,"Loan_FilteredData",startRow=1,startCol=1,header=TRUE)
saveWorkbook(wk)




