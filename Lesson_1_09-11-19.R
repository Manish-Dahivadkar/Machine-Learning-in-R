a=10
a
a<-10
a
a=10
print (a)
a=10
a==11
a==10
num=(1,4,5)
num=c(1,4,5)
num
var=2
var1=3
var+var1
var-var1
var*var1
var1/var
11%%3
15%%3
number1=c(2,4,6,11)
number2=c(7,8,9,5)
number1+number2
number1-number2
number1>number2
number1<number2
number1<number2
number1>numbe
number1==number2
number1=c(2,4,6,11)
number2=c(7,8,9,5)
number1==number2
number2==number2
2^5
2^.5
9^.5
27^.3
27^(1/3)
27^0.33
ab=10.726
round(ab,1)
round(ab,2)
z=1:8
z
z1=4:10
z1
z1
factorial(5)

Id<-c(1,2,3,4,5,6,7,8,9,10)
Age<-c(20,25,30,35,40,45,50,55,60,65)
salary<-c(100,101,99,200,204,305,208,209,105,210)
Experience<-c(2,3,5,8,12,19,17,13,24,26)
emp_dtl<-data.frame(Id,Age,salary,Experience)
print(emp_dtl)
dim(emp_dtl)
nrow(emp_dtl)
ncol(emp_dtl)
head(emp_dtl)
tail(emp_dtl)
View(emp_dtl)
colnames(emp_dtl)
class(emp_dtl)
emp_dtl$Id
mean(emp_dtl$Age)
mean(emp_dtl$salary)
median(emp_dtl$Age)
sd(emp_dtl$Age)
emp_dtl[c(1,2,3),c(1,3)]
emp_dtl[,c(2,3)]
emp_dtl[c(1,2)]
emp_dtl[4,colnames(emp_dtl)]
emp_dtl[c(1:4,8:10),colnames(emp_dtl)]
emp_dtl[c(1:4,8:10),]
colnames(emp_dtl)[2]<-"NewAge"
colnames(emp_dtl)
mtcars
summary(mtcars)
dim(mtcars)
cor(mtcars)


#understanding correlation matrx 
num1<-c(2,3,4)
num2<-c(6,9,11)
cor(num1,num2)
cor(num2,num1)

?mtcars
?iris

#if databse has alphabetc then first remove alphtabetic columns.5th colum was alphabetic.
cor(iris)
iris[,c(1:4)]
iris7<-iris[,c(1:4)]
cor(iris7)


#for count of each entry to specific column 
table(mtcars$cyl)
table(mtcars$gear)  

table(mtcars$cyl , mtcars$am)
view(mtcars)
View(mtcars)  

#********************************************************************

#new laerning HOW TO INSTALL PACKAGES

install.packages("dplyr")
library(dplyr)

#*********************************************************
#New topic "MATRIX"
  
aa<-c(45,2,3,55,6,99,577,442,234,00)

matt<-matrix(aa,nrow=2,ncol=5)
print(matt)


#a+2b+2c=20
#2a+5b+8c=80
#6a+7b+9c=190
AA<-matrix(c(1,2,2,2,5,8,6,7,9),nrow=3, ncol=3,byrow=TRUE)
print(AA)
BB<-matrix(c(20,80,190),nrow=3,ncol=1,byrow=TRUE)
print(BB)
Answer<-solve(AA,BB)
print(Answer)
