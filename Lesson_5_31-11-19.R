##New Lecture
# BAR PLOT

count<-table(mtcars$gear)
count
barplot(count)
barplot(count,xlab="number of gears", ylab = "count of cars by gears", main="Gear vs car count",col=c("orange","green","red"),legend=rownames(count,left))

##*********************************************************************************************************
getwd()
lung<-read.csv("LungCapData.csv")
table(lung$Smoke)
aa<-table(lung$Smoke)
aa
barplot(aa)
barplot(table(lung$Smoke),col="green")

#.................................................................

##by combining two things how to plot barplot
aa<-table(lung$Smoke,lung$Gender)
aa
categories=c("non_smokers","smokers")
barplot(aa,col=c("green","red"),xlab = "Gender")




##..................................................................

##Histogram

hist(mtcars$disp)
aa =  hist(mtcars$disp,breaks = 15)
aa
hist(mtcars$cyl,main="histogram for the cyl dist", ylab = "freq of cyl", xlab = "cyl capacity",col=("red"))

##...example of lungcap

read.csv("LungCapData.csv")
cr1<-read.csv("LungCapData.csv")
hist(cr1$Height)
hist(cr1$Height, breaks=50)

##........................................

cr2<-read.csv("CreditRisk.csv")
hist(cr2$ApplicantIncome)
hist(cr2$ApplicantIncome,breaks=50)
aa<-hist(cr2$ApplicantIncome,breaks=50)

#.....................

id<-c(1:10)
salary<-c(10,11,12,14,200,300,200,400,111,1000)
empdtl<-data.frame(id,salary)
empdtl
newemp<-c(11,456)
new_empdtl<-rbind(empdtl,newemp)#..to add new row in existing dataset
new_empdtl
#..add new column

wrkexp<-c(1,2,3,4,5,6,10,11,2,10)#..to add to new column in existing dataset
aaa<-cbind(empdtl,wrkexp)
aaa#..print to see new added column



#......................................................
##Linear Regression model
dim(mtcars)
model1<-lm(mpg~.,data=mtcars)
summary(model1)

model2<-lm(mpg~cyl+gear,data=mtcars)
model2
summary(model2)


model3<-lm(mpg~wt+disp+qsec+am+hp,data=mtcars)
model3
summary(model3)

model4<-lm(mpg~wt+qsec+am,data=mtcars)
model4
summary(model4)

model5<-lm(mpg~wt+qsec,data=mtcars)
model5
summary(model5)


model6<-lm(mpg~wt+carb,data=mtcars)
model6
summary(model6)


model6<-lm(mpg~wt+am,data=mtcars)
model6
summary(model6)



##best model is below one with 5 vaiables

model7<-lm(mpg~wt+disp+qsec+am+hp,data=mtcars)
model7
summary(model7)


ped<-predict(model7,mtcars)
ped

actaul<-data.frame(ped,mtcars$mpg)
actaul


mtcars




















































































































































