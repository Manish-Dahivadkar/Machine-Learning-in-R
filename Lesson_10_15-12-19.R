#..Date: 15-Dec-2019
#will learn new algorithm''

#...NAIVE BAYES

getwd()
ctg<-read.csv('CTG.csv')

library(e1071)
ctg$NSP<-factor(ctg$NSP)
ctg_sample<-sample(2, nrow(CTG),replace = TRUE, prob = c(0.8,0.2))
ctg_train<-CTG[ctg_sample==1,]
ctg_test<-CTG[ctg_sample==2,]
AA<-ctg_train[ctg_train$NSP==2,]
bb<-ctg_train[ctg_train$NSP==3,]
cc<-ctg_train[ctg_train$NSP==1,]
ctg_train1<-unique(cc)

ctg_train_new<-rbind(ctg_train1,AA,bb,bb)##..oversampling to improve class imbalnce




ctg_NB<-naiveBayes(NSP~.,LB+AC+FM,data=ctg_train_new)

pred_act_naive<-predict(ctg_NB,ctg_test)
pred_act_naive
tab_naive<-table(pred_act_naive,ctg_test$NSP)
tab_naive
sum(diag(tab_naive))/sum(tab_naive)*100


#...now build model on random forest to see whther accuracy have been improved or not

Model_Ranfor_ctg<-randomForest(NSP~.,data=ctg_train_new)
predict_value_ctg<-predict(Model_Ranfor_ctg,ctg_test)
predict_value_ctg
tab2<-table(predict_value_ctg,ctg_test$NSP)
tab2
acc<-sum(diag(tab2))/sum(tab1)*100
acc

Model_Ranfor_ctg$importance#...this helps in identifying most important varible..higher the score important the variable
#practice at home


install.packages("Boruta")
install.packages("mlbench")


#..what is feature selction check inportance code in above  section.

library(Boruta)#..imp for feature selection in classificationmodels
library(mlbench)

data("Sonar")
head(Sonar)
dim(Sonar)
?Sonar

mdl_bor<-Boruta(Class~.,data=Sonar)  
mdl_bor$finalDecision
##..Confirmed =important, 
#Rejected=Not important, 
#Tentative=Boruta/ model not abale to decide whether important or not

mdl_bor<-Boruta(Class~.,data=Sonar, maxRuns=400)#..maxRuns is for to give number of runs and helps in reducing Tentatives
mdl_bor$finalDecision
plot(mdl_bor)
#...............................................................................................................


install.packages("ISLR")#..fordataset
install.packages("leaps")#..for imp  feature selction for regression model

library(leaps)
library(ISLR)

dim(Hitters)
summary(Hitters)
?Hitters

mdl_hit<-regsubsets(Salary~.,Hitters)
summary(mdl_hit)

mdl_hit<-regsubsets(Salary~.,Hitters, nvmax=19)
summary(mdl_hit)

lcn<-read.csv("LungCapData.csv")
head(lcn)
mdl_lcn<-regsubsets(LungCap~.,data=lcn)
summary(mdl_lcn)


#.....................................................................................................................

#Cross validation Technique


library(caret)
library(e1071)

View(iris)
iris$Species<-factor(iris$Species)

iris_sample<-sample(2,nrow(iris), replace=TRUE,prob=c(0.8,0.2))
iris_train<-iris[iris_sample==1,]
iris_test<-iris[iris_sample==2,]

controlParameters1<-trainControl(method="cv",number=7)

modelrandom_iris_NB<-train(Species~.,
                           data=iris_train,
                           method="naive_bayes",
                           trcontrol=controlParameters1,
                           tunegrid=NULL)


modelrandom_iris_pred_NB<-predict(modelrandom_iris_NB,iris_test$Species)

#...method="randomForest'..for random forest
#...method="rpart"  for decision tree
#...method="svmLinear"
#....method="glm"...for logistic

#...assignment by using above build all mode  