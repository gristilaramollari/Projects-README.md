#installing required pacakges
#install.packages("arules") 
#install.packages("arulesViz") 
#install.packages("tweenr")
#install.packages("graphlayouts")


#importing the libraries 
library("arules") 
library("arulesViz")

   

#import the dataset transcation  
tr_data<- read.transactions("~/ubiqum/Module3/Task 1/Task_1/.ipynb_checkpoints/ElectronidexTransactions2017.csv", sep=",", format = "basket", rm.duplicates = TRUE) 

#As we have a large database we are going to take a subset of the data by selecting 1000 observations 
Sample_data<- sample(tr_data,1000) 


#Getting to know with our transcational data 
#Getting information from the transcations 

summary(tr_data)


#Plotting the most common items 
itemFrequencyPlot(tr_data, topN= 5)


#Visualizing all the transcation within our dataset 
image(sample(tr_data,100)) #sampling a little amount of transcations in order to see the image much more clear.

#Checking the indvidual transaction 

inspect(tr_data[5,])

#Verifyng the number of items per transaction 
size(tr_data)[1:100]  




#### NOTE: BELOW I HAVE USED TO SELECT ONLY ONE ITEM TRANSACTION FROM THE TRANSACTION FILE USING THE LIBRARIES ARULES AND ARULESVIZ. 
################################################################################################ 


#### FILTERING THE DATA#### 
#We can use the size function to keep only one item per transcation 
data_1item<- tr_data[which(size(tr_data) == 1),] 
data_1item

#Now lets create a crosstable for this 1 item table (INSTEAD OF SCATERPLOTTING)
myCross_t<- crossTable(data_1item) 
myCross_t[10:14, 10:14]

#Now we can find also the most isolated sold product 
itemFrequencyPlot(data_1item,topN= 1)  

# We can find how many times a  single product was sold. 
myCross_t["Apple MacBook Air", "Apple MacBook Air"] 
myCross_t["iMac", "iMac"] 
myCross_t["HP Laptop", "HP Laptop"] 
myCross_t["CYBERPOWER Gamer Desktop","CYBERPOWER Gamer Desktop"] 
myCross_t["Apple Earpods", "Apple Earpods"]


####Applying apriori algorithm 
gr_rules<- apriori(tr_data)
#### Note: As we can see from console we have 0 rules so we have to set parameters 
#Targeting items by generating rules
gr_rules<- apriori(tr_data, parameter = list(supp=.001, conf= .8)) 
 
#Checking the first five rules  
inspect(gr_rules[1:5]) 

#Sorting the rules  in order to get the most relevant rules or the most likely rules. 
gr_rules<- sort(gr_rules, by= "support", decreasing = T)
inspect(gr_rules[1:10]) 

# Finding the redundant rules 
redundant_rules<- is.redundant(gr_rules) 
redundant_rules 
summary(redundant_rules)  

#Dropping the redudant rules(those rules that are repeated more than once.)
gr_rules<- gr_rules[!redundant_rules]
gr_rules
inspect(gr_rules[1:5])  

#Changing the sides.
gr_rules_iMac<- apriori(tr_data, parameter = list(supp=.0001, conf=.08), appearance = list(default="rhs", lhs="iMac"))
inspect(gr_rules_iMac[1:10]) 
 #Plotting rules 
plot(gr_rules, method = "graph", engine = "interactive") 
gr_rules<- apriori(tr_data, parameter= list(supp=.0001, conf= .08), appearance = list(default="rhs", lhs="iMac"))
#The following teqniques works better with a few rules.
subrules <- sample(gr_rules, 5) 
plot(subrules, method = "graph", engine = "interactive")  
 
 
