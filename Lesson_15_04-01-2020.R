#....New lecture...04-Jan-2020

install.packages("ROSE")
library(ROSE)

cr<-read.csv("CreditRisk.csv")
head(cr)

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

head(Train_CRS)


#....AUROC...HOW TO BUILD

Model_CR1<-glm(Loan_Status1~Credit_History,family = binomial, data=Train_CRS[,c(2:13)])
pred_glm <-predict(Model_CR1, Train_CRS, type= "response")


pred_glm<-na.omit(pred_glm)
pred_actual<-data.frame(pred_glm,Train_CRS$Loan_Status1)
library(dplyr)
pred_actual<-mutate(pred_actual,pred_val=ifelse(pred_glm>0.5,1,0))
pred_actual<-na.omit(pred_actual)

colnames(pred_actual)[3]<-"predicted"
colnames(pred_actual)[2]<-"actual"


roc.curve(pred_actual$actual, pred_glm)#...need to check code carefully not good curve


#..Oversampling and Undersamplin (check code in notebook)
cr<-read.csv("CreditRisk.csv")




#............................................................................................

#..Natural language processing

install.packages("tm")
install.packages("wordcloud")
install.packages("stringr")

library(tm)
library(stringr)
library(wordcloud)

aa<-readLines("MrModi_Speech_IndependenceDay_20171.txt")
aa

text<-paste(readLines("MrModi_Speech_IndependenceDay_20171.txt"),collapse = "")
text

text<-tolower(text)##...convert to lower
text

print(stopwords())#...have just seen the stopword

class(stopwords())

text2<-removeWords(text,stopwords())
text2

bag_of_word1<-str_split(text2," ")
bag_of_word1

str(bag_of_word1)##...it tells that its list or not

bag_of_word1<-unlist(bag_of_word1)

wordcloud(bag_of_word1,min.freq = 15)
