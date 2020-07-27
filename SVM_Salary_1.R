salary_train<-read.csv(file.choose())
salary_test<-read.csv(file.choose())
View(salary_train)
View(salary_test)
str(salary_train)
str(salary_test)
summary(salary_train)
summary((salary_test))
attach(salary_train)
attach(salary_test)

# Data is already splitted into Train & Test

###### Building SVM models using different Kernel #####

library(kernlab)
library(caret)
?ksvm

# kernel = vanilladot
model_vanilla<-ksvm(Salary~.,data = salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,salary_test)
mean(pred_vanilla==salary_test$Salary) # 84.62%
table(pred_vanilla,salary_test$Salary)
table(salary_test$Salary)

# kernel = rbfdot 
model_rbf<-ksvm(Salary~.,data = salary_train,kernel = "rbfdot")
pred_rbf<-predict(model_rbf,salary_test)
mean(pred_rbf==salary_test$Salary) # 85.43%
table(pred_rbf,salary_test$Salary)
table(salary_test$Salary)

# kernal = besseldot
model_bessel<-ksvm(Salary~.,data = salary_train,kernel = "besseldot")
pred_bessel<-predict(model_bessel,salary_test)
mean(pred_bessel==salary_test$Salary) # 77.03%
table(pred_bessel,salary_test$Salary)
table(salary_test$Salary)

# kernel = polydot
model_poly<-ksvm(Salary~.,data = salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,salary_test)
mean(pred_poly==salary_test$Salary) # 84.62%
table(pred_poly,salary_test$Salary)
table(salary_test$Salary)

## Observation :
# For the above splitted data and use of different kernel techniques,
# "rfbdot" model suits better ; based on the accuracy 85.43%.

