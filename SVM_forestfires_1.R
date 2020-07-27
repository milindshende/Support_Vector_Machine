ff<-read.csv(file.choose())
View(ff)
ff<-ff[-c(1:2)]
str(ff)
summary(ff)
attach(ff)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

ff_norm<-as.data.frame(lapply(ff[1:28],FUN=normalize))
ff_norm<-cbind(ff_norm,ff[29])

######## Split data into Train & Test by Random split method (80/20 ratio) ######

library(caTools)
split<-sample.split(ff_norm$size_category,SplitRatio = 0.80)
split
table(split) # F=104 , T=413 Just to check the split ratio

ff_train<-subset(ff_norm,split==TRUE)
ff_test<-subset(ff_norm,split==FALSE)

###### Building models using different Kernel #####

library(kernlab)
library(caret)

# kernel = vanilladot
model_vanilla<-ksvm(size_category~.,data = ff_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,ff_test)
mean(pred_vanilla==ff_test$size_category) # 97.11%
table(pred_vanilla,ff_test$size_category)
table(ff_test$size_category)

# kernel = rbfdot 
model_rbf<-ksvm(size_category~.,data = ff_train,kernel = "rbfdot")
pred_rbf<-predict(model_rbf,ff_test)
mean(pred_rbf==ff_test$size_category) # 80.76%
table(pred_rbf,ff_test$size_category)
table(ff_test$size_category)

# kernal = besseldot
model_bessel<-ksvm(size_category~.,data = ff_train,kernel = "besseldot")
pred_bessel<-predict(model_bessel,ff_test)
mean(pred_bessel==ff_test$size_category) # 60.57%
table(pred_bessel,ff_test$size_category)
table(ff_test$size_category)

# kernel = polydot
model_poly<-ksvm(size_category~.,data = ff_train,kernel = "polydot")
pred_poly<-predict(model_poly,ff_test)
mean(pred_poly==ff_test$size_category) # 97.11%
table(pred_poly,ff_test$size_category)
table(ff_test$size_category)

## Observation :
# For the above splitted data and use of different kernel techniques,
# "Vanilla" & "Polydot" suits better ; based on the accuracy 97.11% each.

