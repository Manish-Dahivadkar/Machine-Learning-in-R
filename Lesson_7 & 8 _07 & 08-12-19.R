#..This was previous lecture assignment..need to complete
getwd()
pa<-read.csv("Property_Price_Train.csv")
summary(pa)
dim(pa)
names(pa)
col_tobe_dropped<-c("Lane_Type","Fireplace_Quality","Pool_Quality","Fence_Quality","Miscellaneous_Feature")
pa<-pa[,!(names(pa)%in%col_tobe_dropped)]
dim(pa)
summary(pa)
colnames(pa)


#.......................................................................................
library(caret)
iris1<-iris
dmy<-dummyVars("~.",data=iris1)
new_iris<-data.frame(predict(dmy,newdata=iris1))
print(new_iris)
View(new_iris)

pa<-read.csv("Property_Price_Train.csv")
pa1<-pa
ncol(pa1)
summary(pa1)

dropcolumns<-c("Id","Lane_Type","Fireplace_Quality","Pool_Quality","Fence_Quality","Miscellaneous_Feature")
new_pa1<-pa1[,!(names(pa1)%in%dropcolumns)]
summary(new_pa1)

new_pa2<-na.omit(new_pa1)
summary(new_pa2)
nrow(new_pa2)


dmy<-dummyVars("~.",data=new_pa2)
new_pa3<-data.frame(predict(dmy,newdata=new_pa2))
View(new_pa3)
colnames(new_pa3)
ncol(new_pa3)



newpa3_sample<-sample(2,nrow(new_pa3), replace=TRUE,prob = c(0.8,0.2))##..random sampling for test and train data
newpa3_Train<-new_pa3[newpa3_sample==1,]
newpa3_Test<-new_pa3[newpa3_sample==2,]

model1<-lm(Sale_Price~.,data=newpa3_Train)
summary(model1)
rm(pred)
rm(pred_actual)
pred<-predict(model1,newpa3_Test)
pred_actual<-data.frame(pred,newpa3_Test$Sale_Price)
View(pred_actual)


error<-newpa3_Test$Sale_Price
error

error_sq<-error^2
error_sq

#....................................................

#Logistic Regression
getwd()

Credit_Risk<-read.csv("CreditRisk.csv",na.strings="")
summary(Credit_Risk)

var1<-mean(Credit_Risk$LoanAmount,na.rm=TRUE)
Credit_Risk$ApplicantIncome[is.na(Credit_Risk$LoanAmount)]<-var1


var3<-mean(Credit_Risk$ApplicantIncome,na.rm=TRUE)
Credit_Risk$ApplicantIncome[is.na(Credit_Risk$ApplicantIncome)]<-var3



var4<-mean(Credit_Risk$CoapplicantIncome,na.rm=TRUE)
Credit_Risk$CoapplicantIncome[is.na(Credit_Risk$CoapplicantIncome)]<-var4

library(dplyr)

Credit_Risk<-mutate(Credit_Risk,Loan_Status1=ifelse(Loan_Status=="Y",1,0))
Credit_Risk<-mutate(Credit_Risk,Education1=ifelse(Education=="Graduate",1,0))
Credit_Risk<-mutate(Credit_Risk,Self_Employed1=ifelse(Self_Employed=="Yes",1,0))
Credit_Risk<-mutate(Credit_Risk,Property_Area1=ifelse(Property_Area=="Urban",2,ifelse(Property_Area=="Rural",0,1)))
Credit_Risk<-mutate(Credit_Risk,Gender1=ifelse(Gender=="Female",1,0))
Credit_Risk<-mutate(Credit_Risk,Married1=ifelse(Married=="No",1,0))
dim(Credit_Risk)
Credit_Risk<-na.omit(Credit_Risk)
Credit_Risk<-select(Credit_Risk,1,4,7,8,9,10,11,14,15,16,17,18,19)
Credit_Risk
colnames(Credit_Risk)
View(Credit_Risk)

#factorise the data of loan status
Credit_Risk$Loan_Status1<-factor(Credit_Risk$Loan_Status1)

CRS<-sample(2,nrow(Credit_Risk), replace=TRUE, prob=c(0.8,0.2))
Train_CRS <-Credit_Risk[CRS==1,]
Test_CRS<- Credit_Risk[CRS==2,]
dim(Train_CRS)
dim(Test_CRS)


Model_CR1<-glm(Loan_Status1~.,family = binomial, data=Train_CRS[,c(2:13)])
summary(Model_CR1)

pred_glm <-predict(Model_CR1, Test_CRS[,c(2:13)], type= "response")
pred_glm

pred_actual<-data.frame(pred_glm,Test_CRS$Education1)
pred_actual<-mutate(pred_actual,pred_val=ifelse(pred_glm>0.5,1,0))
pred_actual<-na.omit(pred_actual)
colnames(pred_actual)[3]<-"predicted"
colnames(pred_actual)[2]<-"actual"

table(pred_actual$predicted,pred_actual$actual)

#...now build model using significant parameters only

getwd()

Credit_Risk<-read.csv("CreditRisk.csv",na.strings="")
summary(Credit_Risk)

var1<-mean(Credit_Risk$LoanAmount,na.rm=TRUE)
Credit_Risk$ApplicantIncome[is.na(Credit_Risk$LoanAmount)]<-var1


var3<-mean(Credit_Risk$ApplicantIncome,na.rm=TRUE)
Credit_Risk$ApplicantIncome[is.na(Credit_Risk$ApplicantIncome)]<-var3



var4<-mean(Credit_Risk$CoapplicantIncome,na.rm=TRUE)
Credit_Risk$CoapplicantIncome[is.na(Credit_Risk$CoapplicantIncome)]<-var4

library(dplyr)

Credit_Risk<-mutate(Credit_Risk,Loan_Status1=ifelse(Loan_Status=="Y",1,0))
Credit_Risk<-mutate(Credit_Risk,Education1=ifelse(Education=="Graduate",1,0))
Credit_Risk<-mutate(Credit_Risk,Self_Employed1=ifelse(Self_Employed=="Yes",1,0))
Credit_Risk<-mutate(Credit_Risk,Property_Area1=ifelse(Property_Area=="Urban",2,ifelse(Property_Area=="Rural",0,1)))
Credit_Risk<-mutate(Credit_Risk,Gender1=ifelse(Gender=="Female",1,0))
Credit_Risk<-mutate(Credit_Risk,Married1=ifelse(Married=="No",1,0))
dim(Credit_Risk)
Credit_Risk<-na.omit(Credit_Risk)
Credit_Risk<-select(Credit_Risk,1,4,7,8,9,10,11,14,15,16,17,18,19)
Credit_Risk
colnames(Credit_Risk)
View(Credit_Risk)
table(Credit_Risk$Loan_Status1)

#factorise the data of loan status
Credit_Risk$Loan_Status1<-factor(Credit_Risk$Loan_Status1)

CRS<-sample(2,nrow(Credit_Risk), replace=TRUE, prob=c(0.8,0.2))
Train_CRS <-Credit_Risk[CRS==1,]
Test_CRS<- Credit_Risk[CRS==2,]
dim(Train_CRS)
dim(Test_CRS)


Model_CR1<-glm(Loan_Status1~Credit_History,family = binomial, data=Train_CRS[,c(2:13)])
summary(Model_CR1)

pred_glm <-predict(Model_CR1, Test_CRS[,c(2:13)], type= "response")
pred_glm

pred_actual<-data.frame(pred_glm,Test_CRS$Education1)
pred_actual<-mutate(pred_actual,pred_val=ifelse(pred_glm>0.5,1,0))
pred_actual<-na.omit(pred_actual)
colnames(pred_actual)[3]<-"predicted"
colnames(pred_actual)[2]<-"actual"

table(pred_actual$predicted,pred_actual$actual)

#......OVERSAMPLING
colnames(Train_CRS)

CRS<-sample(2,nrow(Credit_Risk), replace=TRUE, prob=c(0.8,0.2))
Train_CRS <-Credit_Risk[CRS==1,]
AB<-Train_CRS[Train_CRS$Loan_Status1==0,]
BB<-Train_CRS[Train_CRS$Loan_Status1==0,]
Train_CRS<-rbind(Train_CRS,AB,BB)
Test_CRS<- Credit_Risk[CRS==2,]
table(Train_CRS$Loan_Status1)
dim(Train_CRS)
dim(Test_CRS)


Model_CR1<-glm(Loan_Status1~.,family = binomial, data=Train_CRS[,c(2:13)])
summary(Model_CR1)

pred_glm <-predict(Model_CR1, Test_CRS[,c(2:13)], type= "response")
pred_glm

pred_actual<-data.frame(pred_glm,Test_CRS$Education1)
pred_actual<-mutate(pred_actual,pred_val=ifelse(pred_glm>0.5,1,0))
pred_actual<-na.omit(pred_actual)
colnames(pred_actual)[3]<-"predicted"
colnames(pred_actual)[2]<-"actual"

table(pred_actual$predicted,pred_actual$actual)

#...................
#..New logistic regression model on Titanic dataset

getwd()
Titanic <-read.csv("Titanic.csv")
summary(Titanic)
dim(Titanic)

var1<-mean(Titanic$survived,na.rm=TRUE)
Titanic$survived[is.na(Titanic$survived)]<-var1


var2<-mean(Titanic$age,na.rm=TRUE)
Titanic$age[is.na(Titanic$age)]<-var2


var3<-mean(Titanic$sibsp,na.rm=TRUE)
Titanic$sibsp[is.na(Titanic$sibsp)]<-var3

var4<-mean(Titanic$parch,na.rm=TRUE)
Titanic$parch[is.na(Titanic$parch)]<-var4

var5<-mean(Titanic$fare,na.rm=TRUE)
Titanic$fare[is.na(Titanic$fare)]<-var5

var6<-mean(Titanic$body,na.rm=TRUE)
Titanic$body[is.na(Titanic$body)]<-var6


Titanic_new<-na.omit(Titanic)
library(dplyr)


Titanic_new<-mutate(Titanic_new,sex1=ifelse(sex=="male",1,0))
dim(Titanic_new)

Titanic_new<-Titanic_new[,c(1,2,5:7,9,11,13,15)]

TTT<-sample(2,nrow(Titanic_new), replace=TRUE, prob=c(0.8,0.2))
Train_TTT <-Titanic_new[TTT==1,]
Test_TTT<- Titanic_new[TTT==2,]
dim(Train_TTT)
dim(Test_TTT)
View(Train_TTT)
Model_TT<-glm(survived~.,family = binomial, data=Train_TTT)
summary(Model_TT)

pred_glm <-predict(Model_TT, Test_TTT, type= "response")
pred_glm

pred_actual<-data.frame(pred_glm,Test_TTT$survived)
pred_actual<-mutate(pred_actual,pred_val=ifelse(pred_glm>0.5,1,0))
pred_actual<-na.omit(pred_actual)
colnames(pred_actual)[3]<-"predicted"
colnames(pred_actual)[2]<-"actual"

table(pred_actual$predicted,pred_actual$actual)

