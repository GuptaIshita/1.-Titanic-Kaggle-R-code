install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
setwd("C:\\Users\\risin\\OneDrive\\Desktop\\jigsaw academy\\ZC. Public Projects_Kaggle_Github\\1. Titanic-Kaggle-R-code\data sets")
titanic=read.csv("train.csv")

#################################univariate analysis###########################################
head(titanic)

str(titanic)
# Continuous - Fare, Age
# Categorical - Name, Sex, Ticket, Cabin, Embarked
# Integer -  PassengerId, Survived, PClass, Sibsp, Parch
# Name, Ticket, Cabin - many levels (>100)
# Only Sex, Embarked - 2, 4 levels respectively

summary(titanic)
# Males are 2 times higher than females
# Age - 177 NA's
# SibSp - Mostly 0, & Maximum - 8
# Parch - Mostly 0, & Maximum - 6
# Fare amount varies from 8 t0 512 
# Mostly to embark on Southampton, then Cherbourg and few on Queenstown (S->C->Q)
# Embarked - 2 NA's

#################################Treatment of missing values##########################################
# Age : imputed with mean i.e. 29.6 = approx 30
# Embarked : imputed with majority i.e. S

titanic$Age[is.na(titanic$Age)]<-30
summary(titanic)
#Age varies from 0.42-80 years. Majority of people are more thn 35 years old

#################################bivariate analysis###########################################
#################################Categorical vs Categorical###################################

#table(titanic$Name, titanic$Sex)
#table(titanic$Name, titanic$Ticket)
#table(titanic$Name, titanic$Cabin)
#table(titanic$Name, titanic$Embarked)

#table(titanic$Sex, titanic$Name)
#table(titanic$Sex, titanic$Ticket)
#table(titanic$Sex, titanic$Cabin)
table(titanic$Sex, titanic$Embarked)
### At every station(C,Q,S), males are higher than females. 
### At S - Southampton, males are twice as comapared to females

#table(titanic$Ticket, titanic$Name)
#table(titanic$Ticket, titanic$Sex)
#table(titanic$Ticket, titanic$Cabin)
#table(titanic$Ticket, titanic$Embarked)

#table(titanic$Cabin, titanic$Name)
#table(titanic$Cabin, titanic$Sex)
#table(titanic$Cabin, titanic$Ticket)
#table(titanic$Cabin, titanic$Embarked)

#table(titanic$Embarked, titanic$Name)
table(titanic$Embarked, titanic$Sex)
#table(titanic$Embarked, titanic$Ticket)
#table(titanic$Embarked, titanic$Cabin)

### Where many levels in factor variable, difficult to interpret

########################Continuous vs Categorical##############################################
str(titanic)
# Continuous - Age, Fare
# Categorical - NAme, Sex, Emabarked, Cabin, Ticket

####################################(1)boxplot###################################################
summary(titanic$Age)
boxplot(titanic$Age)
#Age varies from 1year to 80 years

summary(titanic$Fare)
boxplot(titanic$Fare)

#Fare ranges from 8-512

ggplot(titanic,aes(y=Age,x=Sex))+geom_boxplot()#Age of Males are more than females 
#ggplot(titanic,aes(y=Age,x=Name))+geom_boxplot()
ggplot(titanic,aes(y=Age,x=Embarked))+geom_boxplot()#More people embarked from C & S
#ggplot(titanic,aes(y=Age,x=Cabin))+geom_boxplot()
#ggplot(titanic,aes(y=Age,x=Ticket))+geom_boxplot()

ggplot(titanic,aes(y=Fare,x=Sex))+geom_boxplot()
#ggplot(titanic,aes(y=Fare,x=Name))+geom_boxplot()
ggplot(titanic,aes(y=Fare,x=Embarked))+geom_boxplot()#More fare has been given for C & S
#ggplot(titanic,aes(y=Fare,x=Cabin))+geom_boxplot()
#ggplot(titanic,aes(y=Fare,x=Ticket))+geom_boxplot()
####################################(2)aggregate###################################################
aggregate(Age~Sex,data=titanic,FUN=mean)  #Age of Males is higher
aggregate(Age~Embarked,data=titanic,FUN=mean)
aggregate(Age~Embarked+Sex,data=titanic,FUN=mean)

aggregate(Fare~Sex,data=titanic,FUN=mean)#Females have given higher fare than males
aggregate(Fare~Embarked,data=titanic,FUN=mean)#At C-more fare is given, then S & later Q(C:S:Q)
aggregate(Fare~Embarked+Sex,data=titanic,FUN=mean)#Within females & males, at C-more fare is given then S and later Q (C:S:Q)
###############################################################################################
#################################Continuous vs Continuous##############################################
str(titanic)
# Continuous - Age, Fare
# Categorical - NAme, Sex, Emabarked, Cabin, Ticket
####################################(1)histogram###################################################
hist(titanic$Age) #Majority of people are from age group 20-30 years
hist(titanic$Fare) #Maximum fare amount given by people is between 8-50

ggplot(titanic,aes(x=Age))+geom_histogram(aes(fill=as.factor(Sex)),position="dodge")

ggplot(titanic,aes(x=Age))+geom_histogram(aes(fill=as.factor(Embarked)),position="dodge")
#in every age group, people embarked in following sequence (C->S->Q)

ggplot(titanic,aes(x=Fare))+geom_histogram(aes(fill=as.factor(Sex)),position="dodge")
#Max amount fare is given from 8-50. As fare amount decreases, more people gave fare. 
#Males have given more fare compared to Females
ggplot(titanic,aes(x=Fare))+geom_histogram(aes(fill=as.factor(Embarked)),position="dodge")
#More fare has been given for S
####################################(1)Scatterplot###################################################

ggplot(titanic,aes(x=Age,y=Fare))+geom_point() # No relation

##############################################################################################
####reducing levels for categorical variables - Ticket, Cabin
####Ticket
head(titanic)
data1=data.frame(text=titanic$Ticket,target=titanic$Survived)
data1
unique(data1$text)
unique(data1$target)
library(rpart)
mod1<-rpart(target~text,data=data1,method='class')
unique(mod1$where)
titanic$Ticket_nodes<-mod1$where
head(titanic)
titanic$Ticket<-ifelse(titanic$Ticket_nodes==2,1,0)
head(titanic)

####Cabin
head(titanic)
data2=data.frame(text=titanic$Cabin,target=titanic$Survived)
data2
unique(data2$text)
unique(data2$target)
library(rpart)
mod2<-rpart(target~text,data=data2,method='class')
unique(mod2$where)
titanic$Cabin_nodes<-mod2$where
head(titanic)
titanic$Cabin<-ifelse(titanic$Cabin_nodes==2,1,0)
head(titanic)

titanic=select(titanic,-Cabin_nodes,-Ticket_nodes)

##############################################################################################
####Converting Continuous variables to Categorical- Age, Fare
#Age
min(titanic$Age)  #0.42
max(titanic$Age)  #80
mean(titanic$Age)  #29.75
median(titanic$Age) #30
head(titanic$Age)
titanic$Age<-ifelse(titanic$Age>=0 & titanic$Age<=30,"max", "min")
#Majority of people are from age group 20-30 years
head(titanic$Age)
titanic$Age<-as.factor(titanic$Age)
titanic$Age<-ifelse(titanic$Age=="max",1,0)
head(titanic$Age)

#Fare
min(titanic$Fare)  #0
max(titanic$Fare)  #513
mean(titanic$Fare)  #32.20
median(titanic$Fare) #14.45
head(titanic$Fare)
titanic$Fare<-ifelse(titanic$Fare>=0 & titanic$Fare<=50,"max", "min")
#As max amount fare is given from 8-50
head(titanic$Fare)
titanic$Fare<-as.factor(titanic$Fare)
titanic$Fare<-ifelse(titanic$Fare=="max",1,0)
head(titanic$Fare)
###############################################################################################
###Converting all variables into numeric form
head(titanic)
#Sex
titanic$Sex<-ifelse(titanic$Sex=="male",1,0)
#Embarked
titanic$Embarked<-ifelse(titanic$Embarked=="C"|titanic$Embarked=="S",1,0)  
#Combined C & S as most of the people embarked from C or S

#####Removing 'Name' column as we have Passenger Id as a unique identity
titanic=select(titanic,-Name)
head(titanic)
#############################################################################################
#split the data into train & test
set.seed(123)
index<-sort(sample(nrow(titanic),nrow(titanic)*0.8))
train<-titanic[index,]
test<-titanic[-index,]
#train - 712 * 12
# test - 179 * 12
############################################################################################
#glm model
names(titanic)
model1=glm(Survived~.,family='binomial',data=train)
summary(model1)
#AIC - 144
#Keeping all variables as all of them are important for analysis
############################################################################################
#Model validations
#1) vif
install.packages("car")
library(car)
vif(model1)
#All variables are < 5, so they are not highly co-related, which is good.

#2) Confusion matrix & Area under Curve
#titanic_test=read.csv("test.csv")
titanic_test=test
prob=predict(model1,newdata=titanic_test,type="response")
head(prob)
install.packages("ROCR")
library(ROCR)
names(titanic_test)
#Plotting ROCR curve
pred=prediction(prob,titanic_test$Survived)
roc=performance(pred,'tpr',"fpr")
plot(roc)
abline(0,1)

#confusion matrix
class=ifelse(prob>=0.6,1,0)
t=table(class,titanic_test$Survived)
t
sum(diag(t))/nrow(titanic_test)
#96% accuracy at 0.6 cut-off

#checking AUC value (the hightest one will be a good model)
auc=performance(pred,"auc")
auc
auc=unlist(slot(auc,"y.values"))
auc    #98% accurate

