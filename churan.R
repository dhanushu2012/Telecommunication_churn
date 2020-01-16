rm(list = ls())

telecom <- read.csv("F:/Data Science/IMS Proschool/Term Project/Telecom_Churn__1_/Telecom Churn/telecom_churn.csv")
str(telecom)
summary(telecom)

#install.packages("DataExplorer")
library(DataExplorer)
#install.packages("dplyr")

library(dplyr)
#null value checking
sum(is.na(telecom))
sapply(telecom, function(x) sum(is.na(x)))

#histogram for all contineos variable
#normally distributed
hist(telecom$total.day.calls)
hist(telecom$total.day.charge)
hist(telecom$total.eve.calls)
hist(telecom$total.eve.charge)
hist(telecom$total.night.calls)
hist(telecom$total.night.charge)
hist(telecom$total.intl.charge)

#skewed
hist(telecom$total.intl.calls)
hist(telecom$customer.service.calls)
hist(telecom$number.vmail.messages)

#Bar plot for categorical variable
library(DataExplorer)
text(barplot(table(telecom$churn),col=c('green','red'),main='Bar plot of Churn')
     ,0,table(telecom$churn),cex=2,pos=3)


str(telecom)

library(ggplot2)
exp2 <- ggplot(telecom, aes(international.plan, fill = churn)) + geom_bar(position = "fill") + labs(x = "International?", y = "") + theme(legend.position = "none")
exp3 <- ggplot(telecom, aes(voice.mail.plan, fill = churn)) + geom_bar(position = "fill") + labs(x = "Voicemail?", y = "") + theme(legend.position = "none") 
exp4 <- ggplot(telecom, aes(customer.service.calls, fill = churn)) + geom_bar(position = "fill") + labs(x = "Customer calls", y = "") + theme(legend.position = "none") 
library(gridExtra)
grid.arrange( exp2, exp3, exp4, ncol = 4, nrow = 1, top = "Churn/Non-churn Proportion")
??grid.arrange

install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)
#md.pattern().  It returns a tabular form of missing value present in each variable in a data set.

md.pattern(telecom)
#create a visual which represents missing values
mice_plot <- aggr(telecom, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(telecom), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


install.packages("Hmisc")
library(Hmisc)
#seed missing values ( 10% )
#see the missing value
#telecom.mis <- prodNA(telecom, noNA = 0.1)
str(telecom)
#imputing mean values
telecom$total.day.calls <- with(telecom, impute(total.day.calls, mean))
telecom$total.day.charge <- with(telecom, impute(total.day.charge, mean))
telecom$total.eve.calls <- with(telecom, impute(total.eve.calls, mean))
telecom$total.eve.charge <- with(telecom, impute(total.eve.charge, mean))
telecom$total.night.calls <- with(telecom, impute(total.night.calls, mean))
telecom$total.night.charge <- with(telecom, impute(total.night.charge, mean))
telecom$total.intl.charge <- with(telecom, impute(total.intl.charge, mean))

summary(telecom)
#imputing median values
telecom$total.intl.calls <- with(telecom, impute(total.intl.calls, median))
telecom$customer.service.calls <- with(telecom, impute(customer.service.calls, median))
telecom$number.vmail.messages <- with(telecom, impute(number.vmail.messages, median))
summary(telecom)

#check null values
sum(is.na(telecom))
sapply(telecom, function(x) sum(is.na(x)))
str(telecom)
#covert to numeric
telecom$total.day.calls=as.numeric(telecom$total.day.calls)
telecom$total.night.charge = as.integer(telecom$total.night.charge)
telecom$total.day.charge = as.numeric(telecom$total.day.charge)
telecom$total.eve.calls = as.integer(telecom$total.eve.calls)
telecom$total.eve.charge = as.numeric(telecom$total.eve.charge)
telecom$total.night.calls = as.integer(telecom$total.night.calls)
telecom$total.intl.charge = as.numeric(telecom$total.intl.charge)
telecom$total.intl.calls = as.integer(telecom$total.intl.calls)
telecom$customer.service.calls = as.integer(telecom$customer.service.calls)
telecom$number.vmail.messages = as.numeric(telecom$number.vmail.messages)



summary(telecom$churn)
#createing new variable
telecom$international_plan2<-ifelse(telecom$international.plan=="yes",1,0)
telecom$voice.mail_plan2<-ifelse(telecom$voice.mail.plan=="yes",1,0)
telecom$churn2 <-ifelse(telecom$churn=="TRUE",1,0)
summary(telecom)
str(telecom)

install.packages("corrplot")

library(corrplot)
#check corelation 
corrplot(cor(telecom[sapply(telecom, is.numeric)]))

final_data<-telecom[-c(1,4,5,6,17,18)]

summary(final_data)

str(final_data)

#Univariate analysis

bx=boxplot(final_data$account.length)
bx$stats
#getting the quantile values
quantile(final_data$account.length,seq(0,1,0.02))
#based on the outliers we are capping above 98%
final_data$account.length<-ifelse(final_data$account.length>=185,185,final_data$account.length)
#check outlier
par(mfrow=c(1,2))
bx2=boxplot(final_data$area.code)
bx3=boxplot(final_data$phone.number)

bx4=boxplot(final_data$number.vmail.messages)
bx4$stats
quantile(final_data$number.vmail.messages,seq(0,1,0.02))
final_data$number.vmail.messages<-ifelse(final_data$number.vmail.messages>=50,50,final_data$number.vmail.messages)


bx5=boxplot(final_data$total.day.calls)
bx5$stats
quantile(final_data$total.day.calls,seq(0,1,0.02))
par(mfrow=c(1,2))
final_data$total.day.calls<-ifelse(final_data$total.day.calls>=141,141,final_data$total.day.calls)
final_data$total.day.calls<-ifelse(final_data$total.day.calls<=59,59,final_data$total.day.calls)


bx6=boxplot(final_data$total.day.charge)
bx6$stats
quantile(final_data$total.day.charge,seq(0,1,0.02))
final_data$total.day.charge<-ifelse(final_data$total.day.charge>=55.20,55.20,final_data$total.day.charge)
final_data$total.day.charge<-ifelse(final_data$total.day.charge<=5.97,5.97,final_data$total.day.charge)

bx7=boxplot(final_data$total.day.charge)

bx8=boxplot(final_data$total.eve.calls)
bx8$stats
quantile(final_data$total.eve.calls,seq(0,1,0.02))
final_data$total.eve.calls<-ifelse(final_data$total.eve.calls>=142,142,final_data$total.eve.calls)
final_data$total.eve.calls<-ifelse(final_data$total.eve.calls<=59,59,final_data$total.eve.calls)

bx9=boxplot(final_data$total.eve.charge)
bx9$stats
quantile(final_data$total.eve.charge,seq(0,1,0.02))
final_data$total.eve.charge<-ifelse(final_data$total.eve.charge>=25.9236,25.9236,final_data$total.eve.charge)
final_data$total.eve.charge<-ifelse(final_data$total.eve.charge<=7.9984,7.9984,final_data$total.eve.charge)

bx10=boxplot(final_data$total.night.calls)
bx10$stats
quantile(final_data$total.night.calls,seq(0,1,0.02))
final_data$total.night.calls<-ifelse(final_data$total.night.calls>=140,140,final_data$total.night.calls)
final_data$total.night.calls<-ifelse(final_data$total.night.calls<=61,61,final_data$total.night.calls)


bx11=boxplot(final_data$total.night.charge)
bx11$stats
quantile(final_data$total.night.charge,seq(0,1,0.02))
final_data$total.night.charge<-ifelse(final_data$total.night.charge>=13.69,13.69,final_data$total.night.charge)
final_data$total.night.charge<-ifelse(final_data$total.night.charge<=4.3992,4.3992,final_data$total.night.charge)

bx12=boxplot(final_data$total.intl.calls)
bx12$stats
quantile(final_data$total.intl.calls,seq(0,1,0.02))
final_data$total.intl.calls<-ifelse(final_data$total.intl.calls>=9,9,final_data$total.intl.calls)


bx13=boxplot(final_data$total.intl.charge)
bx13$stats
quantile(final_data$total.intl.charge,seq(0,1,0.02))
final_data$total.intl.charge<-ifelse(final_data$total.intl.charge>=4.27,4.27,final_data$total.intl.charge)
final_data$total.intl.charge<-ifelse(final_data$total.intl.charge<=1.16,1.16,final_data$total.intl.charge)

bx14=boxplot(final_data$customer.service.calls)
bx14$stats
quantile(final_data$customer.service.calls,seq(0,1,0.02))
final_data$customer.service.calls<-ifelse(final_data$customer.service.calls>=4,4,final_data$customer.service.calls)




#Bivariate analysis
library(car)
scatterplot(final_data$number.vmail.messages,final_data$churn2)
scatterplot(final_data$account.length,final_data$churn2)
scatterplot(final_data$area.code,final_data$churn2)
scatterplot(final_data$total.day.calls,final_data$churn2)
scatterplot(final_data$total.day.charge,final_data$churn2)
scatterplot(final_data$total.eve.calls,final_data$churn2)

scatterplot(final_data$total.eve.charge,final_data$churn2)

scatterplot(final_data$total.night.calls,final_data$churn2)
scatterplot(final_data$total.night.charge,final_data$churn2)


scatterplot(final_data$total.intl.calls,final_data$churn2)
scatterplot(final_data$total.intl.charge,final_data$churn2)
scatterplot(final_data$customer.service.calls,final_data$churn2)

#divide the data into test and train

str(final_data)

library(caTools)
final_data$churn2<-as.factor(final_data$churn2)
final_data$voice.mail_plan2<-as.factor(final_data$voice.mail_plan2l)
str(final_data)
split<-sample.split(final_data,SplitRatio = 0.70)
split
training<-subset(final_data,split=="TRUE")
testing<-subset(final_data,split=="FALSE")
#check the event rate
prop.table(table(training$churn2))
prop.table(table(testing$churn2))

#model preparation
training
library(car)

model<-glm(as.factor(churn2)~.,family ="binomial",data=training)
summary(model)
#checking the non singificat variable and remove
#Residual deviance should not increase and AIC value should be decrease then select that variable

vif(model)

model2<-glm(as.factor(churn2)~. -total.night.calls -total.eve.calls - number.vmail.messages -voice.mail_plan2,family ="binomial",data=training)
summary(model2)
write.csv(model2,"output2.csv", row.names = F)

vif(model2)

#probability
res<-predict(model2,testing)


res
#glm variable importance
library(caret)
varImp(model2)

#confusion Matrix
tab <- table(Actualvalue=testing$churn2,Predictedvalue=res>0.5)
tab
print(paste('Logistic regration Accuracy',sum(diag(tab))/sum(tab)))



library(lattice)
library(ggplot2)
library(caret)
library(e1071)


#testing$churn2<-as.numeric(testing$churn2)
#training$churn2<-as.numeric(training$churn2)
prediction<-ifelse(training$res>=0.5,1,0)
confusionMatrix(res, testing$churn2, positive = "1")
#testing$churn2<-as.factor(testing$churn2)
#training$churn2<-as.factor(training$churn2)

class(testing$churn2)


#Odds Ratio
exp(confint(model2))
confint(model)
library(ROCR)
res<-predict(model2,training,type = "response")
ROCRpred=prediction(res,training$churn2)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
res<-predict(model,testing,type = "response")
write.csv(res,"output2.csv", row.names = F)
abline(a=0, b=1, col="Red")

auc.perf = performance(ROCRpred, measure = "auc")
auc.perf@y.values

#Assessing the predictive ability of the Logistic Regression model

testing$churn2<-as.character(testing$churn2)
testing$churn2[testing$churn2=="No"]<-"0"
testing$churn2[testing$churn2=="Yes"]<-"1"
fitted_result<-predict(model2,newdata=testing,type="response")
write.csv(fitted_result,"output3.csv", row.names = F)
fitted_result<-ifelse(fitted_result>0.3,1,0)
misClasificError<-mean(fitted_result !=testing$churn2)

print(paste('Logistic Regression Accuracy', 1- misClasificError))
#accuracy=0.86

table(testing$Churn, fitted_result > 0.5)
library(MASS)
#Odds Ratio
exp(cbind(OR = coef(model2), confint(model2)))



#decision tree

install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
testing$churn2<-as.factor(testing$churn2)
training$churn2<-as.factor(training$churn2)

#tree <- rpart(churn2~.,data=training,method="class",control=rpart.control(minsplit=30,cp=0.01))
tree <- rpart(churn2~.,data=training,method="class")
plot(tree)
summary(tree)
print(tree)
prp(tree)
plotcp(tree)
printcp(tree)
#decision tree confustion matrix
??confusionMatrix()
library(caret)
predtr<-predict(tree,training,type="class")
confusionMatrix(predtr,training$churn2)
predtr<-predict(tree,testing,type="class")

tab<-table(Predicted = predtr, Actual = testing$churn2)
tab
print(paste('Decision Tree Accuracy',sum(diag(tab))/sum(tab)))
#0.90
varImp(tree)

#random forest
testing$churn2<-as.factor(testing$churn2)
training$churn2<-as.factor(training$churn2)
library(randomForest)

rfModel <- randomForest(churn2 ~., data = training)
print(rfModel)
#Random Forest Prediction and Confusion Matrix
pred_rf <- predict(rfModel, testing)
#caret::confusionMatrix(pred_rf, testing$Churn)
tab3<-table(Predicted = pred_rf, Actual = testing$churn2)
tab3
plot(rfModel)
training$churn2<-as.factor(training$churn2)
confusionMatrix(pred_rf, testing$churn2, positive = "1")
str(testing)

print(paste('randomforest Accuracy',sum(diag(tab3))/sum(tab3)))
#accuracy=0.91
importance(rfModel)
varImpPlot(rfModel)
  

