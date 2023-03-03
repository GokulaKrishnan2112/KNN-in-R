require(caret)

View(iris)
summary(iris)
set.seed(236)

##Randomizing the data#####
ru<-runif(1:nrow(iris))
iris<-iris[order(ru),]
head(iris)

###Scaling functioin#####
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

###Normalizing the data and partitioning#####
iris_norm<-as.data.frame(lapply(iris[,-5], normalize))
summary(iris_norm)
iris_train<- iris_norm[1:129,]
iris_test<- iris_norm[130:150,]
iris_train$Species<-iris$Species[1:129]
iris_test$Species<-iris$Species[130:150]

###KNN Model########
require(class)
train_label = iris$Species[1:129]
test_label = iris$Species[130:150]
m1_knn<-knn(train = iris_train[-5],test = iris_test[-5],cl=train_label,k=round(sqrt(nrow(iris_train))))
table(test_label,m1_knn)
confusionMatrix(m1_knn,test_label) #accuracy - 95.24%

###Cross validation#####
trcontrol<-trainControl(method = "repeatedcv",number = 10,repeats = 3)
set.seed(786)
m1<-train(Species~.,data=iris_train,method='knn',tuneLength=20,trControl=trcontrol)
m1
?train
plot(m1) #plot agains k and accuracy
varImp(m1)

####Testing#######
pred<-predict(m1,newdata=iris_test)
confusionMatrix(pred,iris_test$Species)

#Finetuning##
new_iris_train<-iris_train[,3:5] #selecting only Petal Length and Width along with Species
new_iris_test<-iris_test[,3:5]
m2<-train(Species~.,data=new_iris_train,method='knn',tuneLength=20,trControl=trcontrol)

##Model metrics for fine tuned model###
pred_new<-predict(m2,newdata=new_iris_test)
confusionMatrix(pred_new,new_iris_test$Species)


ctrl<-trainControl(method = "repeatedcv",number = 10,repeats = 3,classProbs = TRUE,summaryFunction = multiClassSummary)
set.seed(78)
#install.packages("MLmetrics")
require(MLmetrics)
#Tune length parameter allows the algorithm to try different default values for the main parameter
m3<-train(Species~.,data=iris_train,method='knn',tuneLength=20,trControl=ctrl,metric="ROC")
m3
plot(m3) #plot agains k and log loss - at K=7,log loss was min and it is used in the model.
varImp(m3)

#Tune grid parameter allows the algorithm to try different values for the main parameter
m4<-train(Species~.,data=iris_train,method='knn',trControl=ctrl,metric="ROC",tuneGrid = expand.grid(k=c(2,5,6,9)))
m4
fin_pred<-predict(m3,newdata = iris_test)
confusionMatrix(fin_pred,iris_test$Species)


