#....29-Dec-2019 #

#...Market Basket (Aprioli algorithm)

install.packages("arules")
library(arules)
data("Groceries")
summary(Groceries)
head(Groceries)
View(Groceries)

rules<-apriori(Groceries, parameter=list(supp=0.001, conf=0.8))
summary(rules)
inspect(rules[1:30])

rules<-apriori(Groceries, parameter=list(supp=0.001, conf=0.8),appearance = list(rhs="other vegetables",default="lhs"))
summary(rules)
inspect(rules[1:30])

rules<-apriori(Groceries, parameter=list(maxlen= 3,supp=0.001, conf=0.8))
summary(rules)
inspect(rules)

rules<-apriori(Groceries, parameter=list(supp=0.001, conf=0.4))
summary(rules)

#................................
getwd()
cos<-read.csv("Cosmetics.csv")

rules_cos<-apriori(cos,parameter = list(supp=0.1, conf=0.8))
summary(rules_cos)

rules_cos<-apriori(cos,parameter = list(supp=0.5, conf=0.8))
summary(rules_cos)
inspect(rules_cos)


rules_cos<-apriori(cos,parameter = list(supp=0.5, conf=0.6),appearance = list(rhs=c("Foundation=Yes"),default="lhs"))
summary(rules_cos)
inspect(rules_cos)


#..............................................................................................................

#..Priciple Component Analysis

mtcars<-data.frame(mtcars)

model1<-lm(mpg~.,data=mtcars)
summary(model1)

#..pca starts here

mydata<-mtcars[,-1]#...not selecting MPG colum
head(mydata)

mydata1<-scale(mydata)
head(mydata1)

mypca<-prcomp(mydata1)
mypca
summary(mypca)

dim(mydata)

head(mydata)

plot(mypca, type="l")

mypca$x

dim(mypca$x)

cor(mypca$x)

mydata_pca<-cbind(mtcars[,1],as.data.frame(mypca$x))

head(mydata_pca)

colnames(mydata_pca)[1]<-"mpg"

lm_pca_mtcars<-lm(mpg~PC1+PC2+PC3, data=mydata_pca)
summary(lm_pca_mtcars)


#.................................................

#...Linear Discremninate Analysis


library(MASS)


View(iris)

iris$Species<-factor(iris$Species)
iris_lda<-lda(Species~.,data=iris)
iris_lda
summary(iris_lda)



iris_lda_pred<-predict(iris_lda,iris)
iris_lda_pred

iris_lda_pred$class


iris_pred_actual_df<-data.frame(iris_lda_pred$class, iris$Species)
tab<-table(iris_pred_actual_df)
tab
sum(diag(tab)/sum(tab))


#................................................
ctg<-read.csv("CTG.csv")
ctg$NSP<-factor(ctg$NSP)

ctg_lda<-lda(NSP~.,data=ctg)
ctg_lda


ctg_lda_pred<-predict(ctg_lda,ctg)
ctg_lda_pred

ctg_lda_pred$class


ctg_pred_actual_df<-data.frame(ctg_lda_pred$class, ctg$NSP)
tab<-table(ctg_pred_actual_df)
tab
sum(diag(tab)/sum(tab))



#...How to save model ..........................................
mtcars<-data.frame(mtcars)

model1<-lm(mpg~.,data=mtcars)
summary(model1)

saveRDS(model1,"Model_mtcars")
model1_loaded<-readRDS("Model_mtcars")

predict(model1_loaded,mtcars)
