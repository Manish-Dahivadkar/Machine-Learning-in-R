-## R-classroom lecture-3 date : 16-11-2019

## Mutate Function = used to add new column in data frame
getwd()##gives defualt path of computer fro R-stuadio
creditrisk<-read.csv("CreditRisk.csv")
creditrisk
colnames(creditrisk)##to read colum names 
library(dplyr)
##When only two conditions present in one column then assign 1 or 2 to each condition
creditrisk<-mutate(creditrisk,Loan_Status1=ifelse(Loan_Status=="Y",1,0))
head(creditrisk)

#.................................................................................

##when multiple conditions presnt , use two times ifelsea as below
table(creditrisk$Property_Area)
creditrisk<-mutate(creditrisk,NewArea=ifelse(Property_Area=="Urban",2,ifelse(Property_Area=="Rural",0,1)))
colnames(creditrisk)
table(creditrisk$NewArea)

#......................................................................

#shortcut for ifelese function is abelow, directly convert alphabetic column to numeric but you can not control the numbers
# as.numeric function
creditrisk$Self_Employed<-as.numeric(creditrisk$Self_Employed)
table(creditrisk$Self_Employed)
creditrisk$Self_Employed

###practice of above code
creditrisk<-mutate(creditrisk,NewGender=ifelse(Gender=="Male",1,0))
table(creditrisk$NewGender)#to check converted values

##.......................................................................................

##Select function example to selct columns by names
##for example we are craeting nnew data frame named as "manish" and in that selecting two column name

manish<-select(mtcars,mpg,cyl)
manish

##........................................................................

##arrange function ..this function will sort your data in column in acending coulumn

arrange_exm <-arrange(mtcars, disp)
arrange_exm

#to arrange in descending order use below code
arrange_exm<-arrange(mtcars,desc(disp))
arrange_exm

#................................................................................

#order function alternate way for arrange function which we laerned above

order(mtcars$disp)

newdata<-mtcars[order(mtcars$disp),]
newdata

#.....................................................................................
# Aggregate function (important function)it is not part of dplyr package
#This function can use to get mean for different categories like different species in iris data base
View(iris)

aggregate(iris$Sepal.Length~iris$Species,iris$Species,mean)

#practice of above code

aggregate(creditrisk$ApplicantIncome~creditrisk$Gender,creditrisk$Gender,mean)

##similarly you can identify median, sum etc. by repalcing mean

##advancment of code ...This code is very important to know mean or other by category wise
aggregate(iris[,c(1:4)],list(iris[,5]),mean)

##.......................................................................................

# Apply FUNCTION

iris1<-iris[,c(1:4)]
apply(iris1,2,mean)    #2 represents column wise #1 will represnt rows
apply(iris1,1,mean)
dim(iris1)

#.................................................................................
#tapply function....t stands for categories
creditrisk$Loan_Status1<-factor(creditrisk$Loan_Status1)

#first understand what is factor 
lnc<-read.csv("LungCapData.csv")# we read new file here
head(lnc)
lnc<-mutate(lnc,NewGender=ifelse(Gender=="male",1,0))#we have assign category
lnc
summary(lnc)##in this NewGender is treated as numeric column

lnc$NewGender<-as.factor(lnc$NewGender)
summary(lnc)

##now come back to tapply
age<-c(50,60,70,80,45,34)
gender<-c("m","f","f","f","m","f")

bbb<-tapply(age,factor(gender),mean)
bbb

tapply(iris$Sepal.Length,iris$Species,mean)

#.......................................................................

#shortcut for selecting column names use atach function

attach(lnc)
Gender
detach(lnc)
#...................................................................

#Unique function ..........................removes duplicates


bb<-c(1,2,3,4,6,8,1,5,9,9,10,6)
cc<-unique(bb) #removes the duplicates
cc

var1<-c(1,2,5,6,6,8,8,"male","male","female","female")
var2<-unique(var1)
var2

#.............................................................................

#New package installation for next activity

install.packages("reshape")
library(reshape)

name<-c("mohan","ravi","manish","atul","shahid","sampat","rummy","manish")
place<-c("goa","mumbai","andaman","agra","goa","mumbai","agra","goa")
spent<-c(100,110,200,150,190,170,180,250)

aa<-data.frame(name,place, spent)

#Cast function after reshape package

aa_cast1<-cast(aa,name~place,value="spent") #will give predictable result
aa_cast1

#............................................................................
#conditional statements

x<-4
if(x>0){print("positive number")}else{print("negative number")}

aa<-9
if(aa<10){print("inside the if part")}else if(a==10){print("inside the else if")}else{print("inside the else part")}

#...............................................................................

#Loops function
#Types of Loops: While loop, For loop, Repeat loop (Forloop is very important)

#First :Repeat Loop

a<-1
repeat{print(a)
  a=a+1
  if(a>4)
    break}

#Second :While loop

i<-1
while (i<10) {print(i)
  print(i*i)
  i=i+1
  
}



#Third: For loop

cars<-c("audi","merc","i10")
for (j in cars) {print(j)}
  





