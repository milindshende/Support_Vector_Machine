ff<-read.csv(file.choose())
View(ff)
ff<-ff[-c(1:2)]
str(ff)
summary(ff)
attach(ff)

######## Split data into Train & Test by sequential method (70/30 ratio) ######

ff_train<-ff[1:362,]
ff_test<-ff[363:517,]

###### Building SVM models using different Kernel #####

library(kernlab)
library(caret)

# kernel = vanilladot
model_vanilla<-ksvm(size_category~.,data = ff_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,ff_test)
mean(pred_vanilla==ff_test$size_category) # 97.41%
table(pred_vanilla,ff_test$size_category)
table(ff_test$size_category)

# kernel = rbfdot 
model_rbf<-ksvm(size_category~.,data = ff_train,kernel = "rbfdot")
pred_rbf<-predict(model_rbf,ff_test)
mean(pred_rbf==ff_test$size_category) # 76.77%
table(pred_rbf,ff_test$size_category)
table(ff_test$size_category)

# kernal = besseldot
model_bessel<-ksvm(size_category~.,data = ff_train,kernel = "besseldot")
pred_bessel<-predict(model_bessel,ff_test)
mean(pred_bessel==ff_test$size_category) # 67.09%
table(pred_bessel,ff_test$size_category)
table(ff_test$size_category)

# kernel = polydot
model_poly<-ksvm(size_category~.,data = ff_train,kernel = "polydot")
pred_poly<-predict(model_poly,ff_test)
mean(pred_poly==ff_test$size_category) # 97.41%
table(pred_poly,ff_test$size_category)
table(ff_test$size_category)

## Observation :
# For the above splitted data and use of different kernel techniques,
# "Vanilla" & "Polydot" suits better ; based on the accuracy 97.41% each.

