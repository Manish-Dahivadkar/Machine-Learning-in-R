#...Date :01-12-2019

getwd()
lungcap<-read.csv("LungCapData.csv")
View(lungcap)
colnames(lungcap)
dim(lungcap)
head(lungcap)
library(dplyr)  #...for using mutate function for creating new column

lungcap<-mutate(lungcap,Smoke1=ifelse(Smoke=="no",0,1))
lungcap<-mutate(lungcap,Gender1=ifelse(Gender=="male",1,0))
lungcap<-mutate(lungcap,Caesarean1=ifelse(Caesarean=="no",0,1))

View(lungcap)

lcn<-lungcap[,c(1,2,3,7,8,9)]
View(lcn)



lcn_sam<-sample(2,nrow(lcn), replace=TRUE,prob = c(0.8,0.2))
lcn_Train<-lcn[lcn_sam==1,]
lcn_test<-lcn[lcn_sam==2,]

model_lcn<-lm(LungCap~.,data=lcn_Train)
summary(model_lcn)   #..................First model with all columns
pred<-predict(model_lcn,lcn_test[,c(2,3,4,5,6)])
View(pred)

model_lcn2<-lm(LungCap~Age+Height+Smoke1+Gender1,data=lcn_Train)  #...by removing with Ceasarean
summary(model_lcn2)

model_lcn3<-lm(LungCap~.,data=lcn_Train[,c(1,2,3,4,5)]) #...another way to select only required columns
summary(model_lcn3)

pred<-predict(model_lcn,lcn_test)

pred<-predict(model_lcn,lcn_test[c(2,3,4,5)])

pred_actual<-data.frame(pred,lcn_test$LungCap)
View(pred_actual)
colnames(pred_actual)[1]<-"Predicted"   #..to change the column names
colnames(pred_actual)[2]<-"Actual"      #..to change the second column name
View(pred_actual)



#...........................................................................................
#..MODEL 1
error<-pred-lcn_test$LungCap
error

error_sq<-error^2
error_sq

MSE<-mean(error_sq)
MSE

RMSE<-MSE^0.5
RMSE

#MAPE
error<-pred-lcn_test$LungCap
error
error_per<-error/lcn_test$LungCap
error_per<-abs(error_per)
MAPE=mean(error_per)*100
MAPE


error<-pred-lcn_Train$LungCap
error
error_per<-error/lcn_Train$LungCap
error_per<-abs(error_per)
MAPE=mean(error_per)*100
MAPE
#Assumptions checking......................................................
model_lcn<-lm(LungCap~.,data=lcn_Train)
summary(model_lcn)
pred_train<-predict(model_lcn,lcn_Train)
pred_train
error_train<-pred_train -lcn_Train$LungCap
plot(error_train)
hist(error_train)
plot(error_train)
abline(h=mean(error_train),col="red")

#............................................................................
#For control charts

plot(mtcars$mpg, type = "b")
abline(h =  20 , col = "red")


mean(mtcars$mpg)

#..............................................................................................

#Build new Linear Regression model on new data set Lung cap data with BMI

getwd()

lungcap_bmi<-read.csv("LungCapData_BMI.csv")
dim(lungcap_bmi)
colnames(lungcap_bmi)

library(dplyr)

lungcap_bmi<-mutate(lungcap_bmi,Smoke1=ifelse(Smoke=="yes",1,0))
lungcap_bmi<-mutate(lungcap_bmi,Gender1=ifelse(Gender=="male",1,0))
lungcap_bmi<-mutate(lungcap_bmi,Caesarean1=ifelse(Caesarean=="yes",1,0))
head(lungcap_bmi)
lungcap_bmi<-lungcap_bmi[,c(1,2,3,7,8,9,10)]
head(lungcap_bmi)

lungcap_bmi_sample<-sample(2,nrow(lcn), replace=TRUE,prob = c(0.8,0.2))##..random sampling for test and train data
lungcap_bmi_Train<-lungcap_bmi[lungcap_bmi_sample==1,]
lungcap_bmi_Test<-lungcap_bmi[lungcap_bmi_sample==2,]


model1<-lm(LungCap~.,data=lungcap_bmi_Train) #...with all variables
summary(model1)

pred<-predict(model1,lungcap_bmi)
View(pred)
pred_actual<-data.frame(pred,lungcap_bmi$LungCap)
View(pred_actual)


#............................................................................................
#Property data
getwd()
pa<-read.csv("Property_Price_Train.csv")
summary(pa)
dim(pa)
names(pa)
col_tobe_dropped<-c("Lane_Type","Fireplace_Quality","Pool_Quality","Fence_Quality","Miscellaneous_Feature")
pa<-pa[,!(names(pa)%in%col_tobe_dropped)]
dim(pa)
summary(pa)



#.............................................................................

#Few important packages for installation

install.packages("MASS")
install.packages("e1071")
install.packages("caret")
