#...User Defined Functions..................

Add_num<-function(a,b){
  print(a)
  print(b)
  num1<-a+b
  num2<<-a-b
  print(num1)
  print(num2)
}

num1<-a+b
Add_num(3,10)
Add_num<-function(a,b){
  print(a)
  print(b)
  num1<-a+b
  num2<<-a-b
  print(num1)
  print(num2)
}

num1
num2
num1
num2

##.............................................................................................
#Plots graphical represenations
xx<-c(1,2,3,4,5,6,7,8,9,10,11,16,19)
yy<-c(1,4,9,16,25,36,49,64,81,100,12,150,250)
plot(xx,yy)


xx<-c(1,2,3,4,5,6,7,8,9,10,11,16,19)
yy<-c(1,4,9,16,25,36,49,64,81,100,12,150,250)
plot(xx,yy,xlab="Numbers", ylab = "Squares",main="Numbers Vs Squares")
plot(xx,yy,xlab="Numbers", ylab = "Squares",main="Numbers Vs Squares",pch="*")


## Pi-chart

GDP<-c(19.4,11.8,4.8,3.4,2.5,2.4)
COUNTRIES<-c("US","CHINA","JAPAN","GERM","UK","INDIA")
GDP_PIE<-pie(GDP,labels = COUNTRIES,main = "GDP Distribution")
sum_gdp=sum(GDP)
per_gdp=GDP/sum_gdp*100
per_gdp=round(per_gdp)
per_gdp_labels=paste(COUNTRIES,per_gdp,print("%"))
per_gdp_labels
GDP_PIE<-pie(GDP,labels = per_gdp_labels,main = "GDP Distribution")
GDP_PIE<-pie(GDP,labels = per_gdp_labels,main = "GDP Distribution",col = rainbow(length(GDP)))##for coloring
legend("topleft",c("US","CHINA","JAPAN","GERM","UK","INDIA"),cex = 0.8,fill = rainbow(length(GDP)))
rm(legend("top",c("US","CHINA","JAPAN","GERM","UK","INDIA"),cex = 0.8,fill = rainbow(length(GDP))))
##...............................................................................................................

#Box-Plot

#.It is always on numeric data

boxplot(mtcars$disp,main='cylinder summary')

##to check the values

var1<-boxplot(mtcars$disp,main='cylinder summary')
var1

boxplot(mtcars$disp,main='cylinder summary',col = "red")


#another example

boxplot(mtcars$disp~mtcars$cyl,main='cylinder Vs Disposition',xlab='No of Cyclinder',ylab='Disposition')
xx1<-boxplot(mtcars$disp~mtcars$cyl,main='cylinder Vs Disposition',xlab='No of Cyclinder',ylab='Disposition')
xx1
colnames(mtcars)
colnames(yy1)
#..to remove outlier 258
mtcars1<-mtcars
mtcars3<-mtcars[!(mtcars$cyl=6 & mtcars$disp==258),]
dim(mtcars3)

##..second way filter function
library(dplyr)
xx<-mtcars
#......................................................


df1<-read.csv("LungCapData.csv")
boxplot(df1$LungCap~df1$Smoke,main=" Lung Cap Vs Smoke", xlab="smoke", ylab = "lungcap")
boxplot(df1$LungCap~df1$Gender,main=" Lung Cap Vs Gender", xlab="Gender", ylab = "lungcap")

colnames(df1)
df2<-filter(df1,Age>=15)
dim(df1)
dim(df2)

boxplot(df2$LungCap~df2$Smoke,main=" Lung Cap Vs Smoke", xlab="Smoke", ylab = "lungcap")

#/..................................Home work

##Build one box plot lungcap vs ceserian
##Buidl one boc plot credit risk
##pi chart (gender & applicant income )
##pi chart of loan status  mean applicant income
##box-plot of lungcap and remove outlier
##box-plot of lungcapacity and age

#..........................................

boxplot(df1$LungCap~df1$Age,main=" Lung Cap Vs Age", xlab="Age", ylab = "lungcap")
