#install.packages("caret", dependencies = c("Depends", "Suggests")) #installing the caret package.  
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("dplyr") 
#install.packages("gbm") 
#install.packages('libcoin', dependencies = T)
#install.packages('C50', dependencies = T)
#install.packages("inum") 
#install.packages("plyr") 
#install.packages("caTools")       # For sampling the dataset
#install.packages("randomForest")  # For implementing random forest algorithm
#install.packages("rpart") 
#install.packages("e1071") 


library("ggplot2")
library("lattice")
library("caret")
library("mlbench") 
library("dplyr")
library("gbm") 
library("C50") 
library("plyr") 
library("caTools") 
library("randomForest") 
library("rpart") 
library("e1071")
# Uploading our data 


#GETTING KNOWN WITH OUR DATA
#View the data 
View(MyData) 

head(MyData) #giving the first observations 
tail(MyData) # giving the last observations 

#Veryfing the structure of our data 
str(MyData) 

#Getting the attributes of our data 
attributes(MyData) 
#Producing a summary of all records on our current  dataset  
summary(MyData) 
#Getting the names of our data objects 
names(MyData)
#Printing instances within the particular columns in our dataset 
MyData$brand 
MyData$salary 
MyData$age 
MyData$elevel
MyData$zipcode 
MyData$car 
MyData$credit  

# Preprocessing our data(data cleaning) 
#Checking if we have any missing value 
is.na(MyData)

#Plotting to check our data visually, checking values expenditures (brand, age, salary)
hist(MyData$brand, col ="blue") # PLOTTING BRAND 
hist(MyData$age, col="red")     #PLOTTING AGE 
hist(MyData$salary, col="green") #PLOTTING SALARY  

#Scatteboxplotting some of our variables, (creating samples because we have a huge volume of data.) 
sample(MyData$salary, 100) 
plot(sample(MyData$salary, 100), col="purple") 
sample(MyData$age,100) 
plot(sample(MyData$age, 100), col= "orange") 

# Evaluating if our data are distributed normally bu using function qqnorm() 
qqnorm(MyData$salary, col= "blue") 
qqnorm(MyData$age, col= "grey") 
qqnorm(MyData$credit, col= "lightblue") 




#Reach into ur dataset and explore our variables 
#Minimum and value of age and salary variable 
min(MyData$age) 
max(MyData$age) 
min(MyData$salary)
max(MyData$salary)
#Checking the range of our two variables 
range(MyData$age) 
range(MyData$salary)  
#Checking the mean of our two variables 
mean(MyData$age) 
mean(MyData$salary)  
#Creating  a new name for our mother database. 
Chart_data <- MyData

#DISCOVERING THE RELATIONSHIP BETWEEN OUR THREE VARIABLES AGE-BRAND-SALARY 
#First,renaming brand variable with the specific name assigned 
Chart_data$brand <- recode(Chart_data$brand, "0"= 'Acer', "1"= "Sony")
#Secondly, creating a datasample 
DataSample<- sample(Chart_data, 100, replace= TRUE) 
#Creating a barplot for age and brand 
ggplot(DataSample, aes(x=brand))+ geom_bar(aes(y=age), stat= 'identity', fill= "forestgreen")+ ylab("age") + ylim(0, 100)
#Creating a barplot for salary and brand 
ggplot(DataSample, aes(x=brand))+ geom_bar(aes(y=salary), stat= 'identity', fill="orange") + ylab("salary") + ylim(0,120000) 
#Creating a barplot for salary and age. 
ggplot(DataSample, aes(x= age, y= salary)) + geom_point(aes(color= factor(brand)))  



 