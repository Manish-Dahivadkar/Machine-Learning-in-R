##....22-12-2019

#..Hypothesis TEST...


getwd()
cr<-read.csv("CreditRisk.csv")
t.test(cr$ApplicantIncome,mu=5130,conf.level = 0.95,alternative = "less")
mean(cr$ApplicantIncome)##...5179.8
sd(cr$ApplicantIncome)
#..HA: Mean applicant income is less ahn 5130

#..H0: Mean applicant income is greater than 5130

t.test(cr$ApplicantIncome,mu=5200,conf.level = 0.95,alternative = "less")
mean(cr$ApplicantIncome)##...5179.8

#..HA: Mean applicant income is less ahn 5200

#..H0: Mean applicant income is greater than 5200

t.test(cr$ApplicantIncome,mu=5700,conf.level = 0.95,alternative = "less")
mean(cr$ApplicantIncome)##...5179.8

#..HA: Mean applicant income is less ahn 5700

#..H0: Mean applicant income is greater than 5700

t.test(cr$ApplicantIncome,mu=5500,conf.level = 0.95,alternative = "less")
mean(cr$ApplicantIncome)##...5179.8

#..HA: Mean applicant income is less ahn 5500

#..H0: Mean applicant income is greater than 5500

t.test(cr$ApplicantIncome,mu=5130,conf.level = 0.95,alternative = "two.sided")

#..HA: Mean applicant income is not equal to 5130 ( can be more  or less)

#..H0: Mean applicant income is equal to 5130 


#...................................................................................

#..Chi-Square Test (type of hypothesis tes)

#..can be used for feature seletion in models

sal_sat<-read.csv("salary_satisfaction.csv")

sal_sat_tbl<-table(sal_sat$Service,sal_sat$Salary)

sal_sat_tbl

chisq.test(sal_sat_tbl) #..p value is less than .05 hene reject null hypo.

#...to check factor relations in credit risk data
getwd()
cr<-read.csv("CreditRisk.csv")

cr1_tbl<-table(cr$Loan_Status,cr$Gender)
chisq.test(cr1_tbl)


#....on emore exmple for chi square test

library(MASS)

View(survey)

AA<-table(survey$Smoke, survey$Exer)
chisq.test(AA)#..P VALUE IS GRETAER THAN 0.05 HENCE THERE IS NO RELATION SHIP


#.................................................

#...ANNOVA test

rest<-read.csv("restuarnt.csv")
View(rest)

rest


rest_strack<-stack(rest)
rest_strack

names(rest_strack)<-c("Salescount","Dishname") #..rename 
rest_strack


rest_annova<-aov(Salescount~Dishname, data=rest_strack)
summary(rest_annova)  ### p = 0.074 > 0.05 hence mean sales is equal





