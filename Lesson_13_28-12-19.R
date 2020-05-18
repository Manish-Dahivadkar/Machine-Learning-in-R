#...Ridge nad Lasso
library(caret)
install.packages("glmnet")
library(glmnet)

#..how to check multicolinearity
getwd()
bodyfat<-read.csv("bodyfat.csv")

model_m_bf<-lm(Bodyfat~.,bodyfat)
summary(model_m_bf)

install.packages("car")
library(car)

vif(model_m_bf)
#........................................................
library(caret)

lamba_seq<-seq(-3,3,length=100)
lamba_seq

model_ridge<-train(
  Bodyfat~.,data=bodyfat, method="glmnet",
  tuneGrid=expand.grid(alpha=0,lambda=lamba_seq)#...for Ridge alpha=0 / for lasso alpha=1
  
                       )
model_ridge
model_ridge$bestTune

coef(model_ridge$finalModel, model_ridge$bestTune$lambda)

model_lasso<-train(
  Bodyfat~.,data=bodyfat, method="glmnet",
  tuneGrid=expand.grid(alpha=1,lambda=lamba_seq)#...for Ridge alpha=0 / for lasso alpha=1
  
)
model_lasso

model_lasso$bestTune

coef(model_lasso$finalModel, model_lasso$bestTune$lambda)

#.........................................
#///MLE method max likelihood

View(cars)
?cars

model_car<-lm(dist~.,data=cars)
model_car
summary(model_car)

library(MASS)

AB<-boxcox(cars$dist~cars$speed)
AB

ab<-data.frame(AB)
View(ab)
which(ab$y==max(ab$y))
ab$x[61]

model_car2<-lm(dist^0.4242~speed,data=cars)#.....inserted lamba value
summary(model_car2)

pred<-predict(model_car2,cars)

pred_actual<-data.frame(pred^1/0.4242,cars$dist)
View(pred_actual)
colnames(pred_actual)[1]<-"Predicted"   #..to change the column names
colnames(pred_actual)[2]<-"Actual"      #..to change the second column name
View(pred_actual)

#...one more practice for above
View(trees)

model_tree<-lm(Volume~.,data=trees)
summary(model_tree)


AB<-boxcox(trees$Volume~., data=trees)
AB

which(AB$y==max(AB$y))
AB$x[58]


model_tree_1<-lm(Volume^0.3030~., data=trees)
summary(model_tree_1)

pred<-predict(model_tree_1,trees)
pred_actual<-data.frame(pred^(1/0.3030),trees$Volume)
View(pred_actual)
colnames(pred_actual)[1]<-"Predicted"   #..to change the column names
colnames(pred_actual)[2]<-"Actual"      #..to change the second column name
View(pred_actual)

?mle
