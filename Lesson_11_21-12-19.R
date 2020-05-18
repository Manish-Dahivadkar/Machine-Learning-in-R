#....Date: 21-12-2019

#..K-Means (unsupervised technique)

getwd()
sns<-read.csv("snsdata.csv")

sns$gender[is.na(sns$gender)]<-"M" #..filling NA values with most frequent entry
sns$age[is.na(sns$age)]<-17.994  #..assigning mean age for NA entries
sns$gender<-as.numeric(sns$gender) #..converting gender column in numeric

model_sns_kmean<-kmeans(sns,3,nstart = 1,iter.max = 10) 

#understanding above syntax
#..in above code sns=dataset
#3= assumed number of k or centroid
#nstart= number of centroid
#itermax= number of iterations 

model_sns_kmean  #..to print model

model_sns_kmean$centers  #..to view centers

View(model_sns_kmean$centers)

#..now to find value of K NEED TO FIND OUT SUM SQURED DISTANCE

model_sns_kmean$tot.withinss

model_sns_kmean<-kmeans(sns,4,nstart = 1,iter.max = 10)
model_sns_kmean$tot.withinss

#..now run itertive model by selecting 7 values of k
num<-seq(1:7)
list1<-list()##empty llist
for (i in num){
  model_sns_kmean<-kmeans(sns,i,nstart = 1,iter.max = 10)
  model_sns_kmean
  model_sns_kmean$tot.withinss
  list1<-append(list1,model_sns_kmean$tot.withinss)
  print(list1)
}

plot(unlist(list1))

#..second way plotting

plot(num,list1, type="b",xlab="Num ofcluster",ylab="Totalsqure distance", col="red") #..type=b connects dots by line

#......New algorith....................................................
#..DBSCAN
install.packages("dbscan")
library("dbscan")


getwd()
sns<-read.csv("snsdata.csv")

sns$gender[is.na(sns$gender)]<-"M" #..filling NA values with most frequent entry
sns$age[is.na(sns$age)]<-17.994  #..assigning mean age for NA entries
sns$gender<-as.numeric(sns$gender) #..converting gender column in numeric

#..normalise data..for distance as we required unitless data

normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}


(sns1<-apply(sns,2,normalize))


sns_dbscan1<-dbscan(sns1,eps=0.5,minPts = 5)
sns_dbscan1
