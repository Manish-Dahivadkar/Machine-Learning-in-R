#....Lecture number 9    date: 14-Dec-2019

#....readline function
number<-readline("Enter the value you want to pass")
number

#...........................................................................................
                              # Decision Tree
install.packages("party")
library(party)
getwd()
CTG<-read.csv('CTG.csv')
View(CTG)
dim(CTG)
#LB: BEATS PER SECOND
#AC: ACCELEARATION PER SEC
#FM: FETAL MOVEMENT PER SECOND
#NSP : normal, suspect, pathological



CTG$NSP<-factor(CTG$NSP)
CTG_SAMPLE<-sample(2, nrow(CTG),replace = TRUE, prob = c(0.8,0.2))
CTG_TRAIN<-CTG[CTG_SAMPLE==1,]
CTG_TEST<-CTG[CTG_SAMPLE==2,]

library(party)

ctg_dt<-ctree(NSP~LB+AC+FM, data=CTG_TRAIN)
pred_value<-predict(ctg_dt,CTG_TEST)
pred_value

tab1<-table(pred_value,CTG_TEST$NSP)
tab1

acc<-sum(diag(tab1))/sum(tab1)*100#...Important code for finding accuracy of  model
acc


#..now we need to control the parameters , so that we acn avoid  overfitting problem

ctg_dt<-ctree(NSP~LB+AC+FM, data=CTG_TRAIN, controls = ctree_control(mincriterion = 0.8, minsplit = 200))
pred_value<-predict(ctg_dt,CTG_TEST)
pred_value

tab1<-table(pred_value,CTG_TEST$NSP)
tab1

acc<-sum(diag(tab1))/sum(tab1)*100
acc



plot(ctg_dt)


#.................................

#........................................................................................

#....Random Forest


install.packages("randomForest")
library(randomForest)

Model_Ranfor_CTG<-randomForest(NSP~.,data=CTG_TRAIN)
predict_value_ctg<-predict(Model_Ranfor_CTG,CTG_TEST)
predict_value_ctg
tab2<-table(predict_value_ctg,CTG_TEST$NSP)
tab2
acc<-sum(diag(tab2))/sum(tab1)*100
acc


Model_Ranfor_CTG<-randomForest(NSP~.,data=CTG_TRAIN, ntree=700)##...ntree parameter for controlling number of trees
predict_value_ctg<-predict(Model_Ranfor_CTG,CTG_TEST)
predict_value_ctg
tab2<-table(predict_value_ctg,CTG_TEST$NSP)
tab2
acc<-sum(diag(tab2))/sum(tab1)*100
acc

#...........................................................................................
#            Support vector machine (SVM)



library(e1071)


Model_SVM_CTG<-svm(NSP~.,data=CTG_TRAIN)
predict_value_ctg<-predict(Model_SVM_CTG,CTG_TEST)
predict_value_ctg
tab2<-table(predict_value_ctg,CTG_TEST$NSP)
tab2
acc<-sum(diag(tab2))/sum(tab1)*100
acc


library(caret)
confusionMatrix(predict_value_ctg,CTG_TEST$NSP)#...FOR CALCULATING TPR, FPR, ACCURACY

#..........................................................................................

#...KNN ( K-Nearest-Neighbour)

library(class)

getwd()
CTG<-read.csv('CTG.csv')
dim(CTG)
#LB: BEATS PER SECOND
#AC: ACCELEARATION PER SEC
#FM: FETAL MOVEMENT PER SECOND
#NSP : normal, suspect, pathological



CTG$NSP<-factor(CTG$NSP)
CTG_SAMPLE<-sample(2, nrow(CTG),replace = TRUE, prob = c(0.8,0.2))
CTG_TRAIN<-CTG[CTG_SAMPLE==1,]
CTG_TEST<-CTG[CTG_SAMPLE==2,]

knn_ctg<-knn(train=CTG_TRAIN,test=CTG_TEST,cl=CTG_TRAIN$NSP,k=5)


pred_act_knn<-data.frame(knn_ctg,CTG_TEST$NSP)
colnames(pred_act_knn)<-c("pred","actual")
tab_knn<-table(pred_act_knn)
tab_knn
sum(diag(tab_knn))/sum(tab_knn)*100


#...home assignment build the model on credithistory data and brast cancer data as well by using all above techniques

#......................................................................................................................
#..HOW TO FIND VALUE OF K
#...KNN ( K-Nearest-Neighbour)

library(class)

getwd()
CTG<-read.csv('CTG.csv')
dim(CTG)
#LB: BEATS PER SECOND
#AC: ACCELEARATION PER SEC
#FM: FETAL MOVEMENT PER SECOND
#NSP : normal, suspect, pathological



CTG$NSP<-factor(CTG$NSP)
CTG_SAMPLE<-sample(2, nrow(CTG),replace = TRUE, prob = c(0.8,0.2))
CTG_TRAIN<-CTG[CTG_SAMPLE==1,]
CTG_TEST<-CTG[CTG_SAMPLE==2,]

knn_ctg<-knn(train=CTG_TRAIN,test=CTG_TEST,cl=CTG_TRAIN$NSP,k=5)


pred_act_knn<-data.frame(knn_ctg,CTG_TEST$NSP)
colnames(pred_act_knn)<-c("pred","actual")
tab_knn<-table(pred_act_knn)
tab_knn
sum(diag(tab_knn))/sum(tab_knn)*100

#...WHICH VALUE OF k WILL GIVE BEST RESULT
num<-seq(1:50)
list1<-list()##empty llist
for (i in num){
  knn_ctg<-knn(train=CTG_TRAIN,test=CTG_TEST,cl=CTG_TRAIN$NSP,k=i)
  pred_act_knn<-data.frame(knn_ctg,CTG_TEST$NSP)
  colnames(pred_act_knn)<-c("pred","actual")
  tab_knn<-table(pred_act_knn)
  print(sum(diag(tab_knn))/sum(tab_knn))
  list1<-append(list1,sum(diag(tab_knn))/sum(tab_knn)*100)
  print(list1)
}

plot(unlist(list1))

