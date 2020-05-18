How to run Array function
vector1=c(2,6,7,11,12,13,20,21,2,3,4,5)
arr1=array(vector1, dim=c(3,2,2))
arr1
****************************************************
List1=list(mtcars,iris)  # this is creating list
List1

#how to print value of variable
a<-50
a
print(a)
print(paste("The value of a is ...",a))

#how to calculate length of dataset or vector
vector1=c(1,2,3,5,6,1,2,5,8,6,4,8,9,2,1)
length(vector1)
name=c("data","science")
nchar(name)

#Sampling:
irissample=sample(2,nrow(iris),replace=TRUE,prob=c(.8,.2))
irissample
dim(iris)
length(irissample)
irissample
irissample=sample(2,nrow(iris),replace=TRUE,prob=c(.8,.2))
irissample
iristrain=iris[irissample==1,]
iristest=iris[irissample==2,]
iristrain
iristest

mtcarssample=sample(2,nrow(mtcars),replace=TRUE,prob=c(.8,.2))
mtcarssample
dim(mtcars)

#How import external data
CR=read.csv("C:/Users/Manish/Desktop/R-classroom/CreditRisk.csv")
head(CR)
View(head(CR))
tail(CR)
class(CR)
summary(CR)
nrow(CR)
ncol(CR)
dim(CR)
View(summary(CR))
table(CR$Gender)

#hOW to convert BLANK to NA'S
cr1=read.csv("C:/Users/Manish/Desktop/R-classroom/CreditRisk.csv",na.strings="")
cr1
View(cr1)
#so if blanks are present in data please convert them to NA's
summary(cr1)

table(cr1$Credit_History)

#whenever null values present need to make assupmtion or can remove the null so craet databse by omitting values
cr2=na.omit(cr1)
summary(cr2)

#or if you have to replace nulls then follow
is.na(cr1$Credit_History) #for identifying nulls in specifi column
cr1$Credit_History[is.na(cr1$Credit_History)]<-0
View(cr1)
summary(cr1)
cr1$LoanAmount[is.na(cr1$LoanAmount)]<-142.5
summary(cr1)

table(cr1$Dependents)


cr1$Gender[is.na(cr1$Gender)]<-"Male"
summary(cr1)
cr1$Self_Employed[is.na(cr1$Self_Employed)]<-"No"
summary(cr1)

cr1$Married[is.na(cr1$Married)]<-"Yes"


cr1$Loan_Amount_Term[is.na(cr1$Loan_Amount_Term)]<-360
summary(cr1)

summary(cr1)
cr1$Dependents[is.na(cr1$Dependents)]<-0
summary(cr1)

#**************************************************************************

quantile(cr1$ApplicantIncome)
max(cr1$ApplicantIncome)
quantile(cr1$ApplicantIncome,prob = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1))
quantile(cr1$ApplicantIncome,prob=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.98,1))
quantile(cr1$ApplicantIncome,prob=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,.98,0.99,1))

#quntile funtion combined with probability helps in finding how the  data is distributed

#************************************************************************************
library(dplyr)
library(dplyr)
cr1<-read.csv("C:/Users/Manish/Desktop/R-classroom/CreditRisk.csv")
#filter function
database1<-filter(cr1,Gender=="Male")
View(database1)
#######
#and function
colnames(cr1)
df2<-filter(cr1,Gender=="Male"& ApplicantIncome>5000)
View(df2)

#or condition
df3<-filter(cr1,Gender=="Male" | ApplicantIncome >5000)
View(df3)
