
#Converting our variables(brand, age, salary) into factors 
MyData$brand<- as.factor(MyData$brand) 
MyData$salary<- as.factor(MyData$salary) 
MyData$age<- as.factor(MyData$age) 
  
#NOTE: WE WILL BE CREATING TRAIN AND TEST SETS. These will be created from the CompleteResponses.csv file

#PARTITIONING DATA 
#Set seed for reproducibility 
#Locks seed for random partitioning 
set.seed(123) 
 

 
#Splitting the data into training and test set. (first we calculate the size of our sets, but we do not create them) 
trainSize<- round(nrow(MyData)*0.75) # define an 75%/25% train/test split of the dataset
testSize<- nrow(MyData)- trainSize  
#Checking how many instances we have per each set 
trainSize 
testSize 

# Creating training and test sets 
training_indices<-sample(seq_len(nrow(MyData)),size =trainSize)
trainingSet<- MyData[training_indices,] 
testingSet<- MyData[-training_indices,]   

M_LOG<- glm(brand~., data= trainingSet, family= "binomial")

#10 fold cross validation 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, savePredictions = TRUE)
 
# Building the SVM model 
# Training the model 
Model<- train(brand~.,data= trainingSet, method= "svmPoly", 
                 na.action = na.omit,
                 trControl=trainControl(method = "none"), 
                 tuneGrid= data.frame(degree=1, scale= 1, C=1)) 

summary(Model) 
 
#Apply the model to make predictions in the training set 
predict1<- predict(Model, trainingSet)
#Apply the model to make predictions in the testing set 
predict2<- predict(Model, testingSet) 
#Perfrom cross validation 
Model<- predict(Model, trainingSet) 


#Model performance(Displays confusion matrix and statistics)
Model_Confusion<- confusionMatrix(predict1,trainingSet$brand) 
#Performing feauture  importance 
Importance<- varImp(Model) 
plot(Importance)


 #Buiding Cv model 
ModelCv= train(brand~., data= trainingSet,method="svmPoly", 
               na.action= na.omit, 
               trControl= trainControl(method="cv",number=2)) 
summary(ModelCv) 
#Apply the model to make predictions in the training set 
Model_training1<- predict(ModelCv, trainingSet)
#Apply the model to make predictions in the testing set 
Model_testing2<- predict(ModelCv, testingSet) 
#Perfrom cross validation 
ModelCv<- predict(ModelCv, trainingSet) 


#Model performance(Displays confusion matrix and statistics)
ModelCv_Confusion<- confusionMatrix(Model_training,trainingSet$brand) 



 
#We'll define the gbm model and include train data to fit the model.
mod_gbm = gbm(brand ~.,
              data = trainingSet,
              cv.folds = 2,
              n.trees = 20) 
summary(mod_gbm) 
#Apply the model to make predictions in the training set 
ModGbm_training1<- predict(mod_gbm, trainingSet)
#Apply the model to make predictions in the testing set 
ModGbm_testing2<- predict(mod_gbm, testingSet) 
#Perfrom cross validation 
mod_gbm<- predict(ModelCv, trainingSet) 


pred<- predict.gbm(object = mod_gbm,
                   newdata = testingSet,
                   n.trees = 50,
                   type = "response")  

#Creating random forest model 
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))
RandomforestModel<- train(brand~., data= trainingSet, method="rf", trControl= fitControl, tunegrid= rfGrid ) 
summary(RandomforestModel) 


#Creating decisionm tree classifier 
tree1<-train(brand~., data= trainingSet, method="rpart", tunelength=10) 



### random forest


rfFit1 <- train(brand~., 
                data = trainingSet, 
                method = "rf", 
                trControl=fitControl, 
                tuneLength = 1,
                ntree=10,
                na.action = na.omit)

 