install.packages("tidyverse")
library(tidyverse)

Heartdisease <- read.csv("Heart_Disease.csv")
head(Heartdisease)
Heartdisease <- Heartdisease %>% mutate(num = ifelse(class > 0, 1, 0))
Heartdisease <- Heartdisease %>% mutate(sex=factor(sex, levels=c("0","1"),labels=c("Female","Male")))
Heartdisease <- subset(Heartdisease, select=-class)
head(Heartdisease)
dim(Heartdisease)
summary(Heartdisease)
box_variables <- subset(Heartdisease, select = c("age","trestbps","chol","thalach","oldpeak","ca","num"))
boxplot(box_variables)

box_variable <- subset(Heartdisease, select = c("age","trestbps","chol","thalach","oldpeak","ca","num","sex"))
box_variable <- box_variable%>%mutate(Label=ifelse(num==1,"Disease","No Disease"))
ggplot(data=box_variable,aes(sex,fill=Label))+geom_bar(position = "fill")+ylab("Values")

as.factor(Heartdisease$num)
Heartdisease$num <- factor(Heartdisease$num, levels = c(0,1), labels = c("No Disease","Disease"))
summary(na.omit(Heartdisease))
logistic.fit <- glm(num ~.-sex, data = Heartdisease, family = binomial)
summary(logistic.fit)
logistic.prob <- predict(logistic.fit,type="response")
logistic.pred <- rep("No Disease.pred", dim(Heartdisease)[1])
logistic.pred[logistic.prob > 0.5] = "Disease.pred"
library(class)
library(MASS)
set.seed(1018)
train<-sample(1:nrow(Heartdisease), nrow(Heartdisease)/2)
Heart_train<-Heartdisease[train,]
Heart_test<-Heartdisease[-train,]
glm.logistic<-glm(num ~.-sex, data = Heart_train, family = binomial)
glm.prob<-predict(glm.logistic,Heart_test,type="response")
glm.pred<-rep(0,dim(Heart_test)[1])
glm.pred[glm.prob>0.5]=1
glm.table<-table(glm.pred,Heart_test$num)
glm.error<-(glm.table[1,2]+glm.table[2,1])/dim(Heart_test)[1]
glm.table
glm.error
fit.lda<-lda(num ~.-sex, data = Heart_train)
pred.lda<-predict(fit.lda,Heart_test)
class.lda<-pred.lda$class
table.lda<-table(class.lda,Heart_test$num)
lda.error<-(table.lda[1,2]+table.lda[2,1])/dim(Heart_test)[1]
table.lda
lda.error
fit.qda<-qda(num ~.-sex, data = Heart_train)
pred.qda<-predict(fit.qda,Heart_test)
class.qda<-pred.qda$class
table.qda<-table(class.qda,Heart_test$num)
qda.error<-(table.qda[1,2]+table.qda[2,1])/dim(Heart_test)[1]
table.qda
qda.error
dim(Heartdisease)
set.seed(1018)
kfolds<-5
folds<-rep_len(1:kfolds,dim(Heartdisease)[1])
folds<-sample(folds,dim(Heartdisease)[1])
glm.test.error<-rep(0,kfolds)
lda_test_error<-rep(0,kfolds)
qda_test_error<-rep(0,kfolds)
for (k in 1:kfolds){
  fold<-which(folds==k)
  Heart1<-Heartdisease[-fold, ]
  Heart2<-Heartdisease[fold, ]
  
  glm.log<-glm(num ~.-sex, data = Heart1, family = binomial)
  glm.test.prob<-predict(glm.log,Heart2,type="response")
  glm.test.pred<-rep(0,dim(Heart2)[1])
  glm.test.pred[glm.test.prob>0.5]=1
  glm.test.table<-table(glm.test.pred,Heart2$num)
  glm.error.test<-(glm.test.table[1,2]+glm.test.table[2,1])/sum(glm.test.table)[1]
  glm.test.error[k]<-glm.error.test
  
  lda_fit <- lda(num ~.-sex, data = Heart1)
  lda_pred <- predict(lda_fit, Heart2)
  lda_class <- lda_pred$class
  lda_table <- table(lda_class,Heart2$num)
  lda_error_test <- (lda_table[1,2] + lda_table[2,1])/sum(lda_table)
  lda_test_error[k] <- lda_error_test
  
  qda_fit <- qda(num ~.-sex, data = Heart1)
  qda_pred <- predict(qda_fit, Heart2)
  qda_class <- qda_pred$class
  qda_table <- table(qda_class,Heart2$num)
  qda_error_test <- (qda_table[1,2] + qda_table[2,1])/sum(qda_table)
  qda_test_error[k] <- qda_error_test
}
glm.test.table
mean(glm.test.error)
lda_table
mean(lda_test_error)
qda_table
mean(qda_test_error)
