#Using  svm classification to create our model  

SVM1<- svm(brand~., data= trainingSet, na.action = na.omit)




summary(SVM1) 
#Apply the model to make predictions in the training set 
Model_training<- predict(SVM1, trainingSet)
#Apply the model to make predictions in the testing set 
Model_testing<- predict(SVM1, testingSet) 
#Perfrom cross validation 
SVM1<- predict(SVM1, trainingSet) 
 

#Model performance(Displays confusion matrix and statistics)
SVM1_Confusion<- confusionMatrix(Model_training,trainingSet$brand) 
 
 
 