library(faraway)
data(pima)
df<-pima
head(df)
summary(df)
df$glucose[df$glucose==0]=NA
df$diastolic[df$diastolic==0]=NA
df$triceps[df$triceps==0]=NA
df$bmi[df$bmi==0]=NA
df$insulin[df$insulin==0]=NA
mean(df$diastolic,na.rm = T)
#ds <- na.omit(df)
#dc<-head(df)
#Insulin = df$insulin[is.finite(df$insulin)]
#Diastolic = df$distolic[is.finite(df$distolic)]
#Glucose = df$glucose[is.finite(df$glucose)]
hist(df$diastolic,xlab = "Diastolic",main = "The sequence of Diastolic",col="Red",border = "black")
hist(df$glucose,xlab = "Glucose",main = "histogram of glucose",col="purple",border = "black")
hist(df$triceps,xlab = "Triceps",main = "histogram of Triceps",col="green",border = "black")
hist(df$bmi,xlab = "BMI",main = "histogram of BMI",col="red",border = "black")
hist(df$insulin,xlab = "Insulin",main = "histogram of Insulin",col="red",border = "black")
#res.ftest <- t.test(test ~ Glucose, data =df)
df$test<-factor(df$test)
levels(df$test)
levels(df$test)<-c("Negative","Positive")
x<-c(sum(df$test=="Negative"),sum(df$test=="Positive"))
pairs(df[,c('glucose','bmi','triceps','insulin')])

par(mfrow=c(2,2))
plot(df$bmi,df$triceps,xlab = 'BMI',ylab = 'Triceps',main='')
plot(df$test,df$distolic,xlab = 'test',ylab = 'Distolic',main='')
hist(df$diastolic,xlab = "Diastolic",main = "The sequence of Diastolic",col="Red",border = "black")
pie3D(x,labels=levels(df$test),explode = 0.2,main="pie chart of test results")
library(plotrix)
pie3D(x,labels=levels(df$test),explode = 0.2,main="pie chart of test results")
plot(df$age,df$diabetes,xlab = "age",ylab="diabites",main = "")
plot(df$test,df$age,ylab="AGE",main='')
library(VIM)
library(mice)
ag<-aggr(df,col=c('black','red'),numbers=T,sortVars=T,labels=names(df),cex.axis=0.7,gap=1,ylab=c("Barplot of missing data","pattern"))
md.pattern(df)
marginplot(df[,c(2,5)])
marginplot(df[,c(4,5)])
marginplot(df[,c(8,4)])
missnum<-function(x){a<-sum(is.na(x))/length(x);return(a)}
missnum(df[1,])
missnum(df[2,])
missnum(df[3,])
missnum(df[4,])
missnum(df[5,])
missnum(df[6,])
missnum(df[8,])
miss<-apply(df,1,missnum)
miss
length(df)
which.max(miss)
max(miss)
which(miss==max(miss))
miss>0.3
mean(miss[miss>0.3])
miss[miss>0.3]
sd(miss[miss>0.3])
length(miss[miss>0.3])
mean(df$age[is.na(df$triceps)])
#********************
library(VIM)
library(mice)
impu<-mice(df,m=10,meth='pmm')
impu$imp$glucose
impu$imp$bmi
impu$imp$triceps
impu$imp$insulin
df[10,]
#fit imus's on data
#use complete function: complete()
com<-complete(impu,1)
head(com)
sum(is.na(com))
#*************
#performance Analysis
xyplot(impu,insulin~triceps+glucose+diastolic| .imp)
xyplot(impu,insulin~triceps+glucose+bmi| .imp)
stripplot(impu)
densityplot(impu)
install.packages("BaylorEdPsych")
library(BaylorEdPsych)
LittleMCAR(df)
