
####################CAPSTONE PROJECT BY SHASHANK TANWAR##########################################################################

#Creating environment
install.packages('ggplot2', dependencies = TRUE)

list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools", "mariotrr"
                      ,"dplyr", "rpart", "randomForest", "sqldf", "caret", "lubridate","tidyr", "stringr" )
new_packages<- list.of.packages[!(list.of.packages %in% installed.packages())]

if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(boot)
library(car)
library(lmtest)
library(sandwich)
library(lubridate)
library(caret)
library(ggplot2)
library(dplyr)
library(MASS)
library(caTools)
#Importing dataset

path <- "C:/Users/Shashank/Downloads/CAPSTONE_PROJECT/01DATA" 
setwd(path)

store_train<- read.csv("train.csv")
str(store_train)
summary(store_train)
dim(store_train)


store_test<- read.csv("test.csv")
str(store_test)
summary(store_test)
dim(store_test)

################################Preprocessing#######################################################

colnames(store_train)
head(store_train)
str(store_train)

#Checking null values
colSums(is.na(store_train))



# correcting ItemFat content column
store_train$Item_Fat_Content<- if_else(store_train$Item_Fat_Content %in% c('reg', 'Regular'), 'Regular', 'Low' )


#Filling missing weight with catogery mean
match_table<- tapply(store_train$Item_Weight, store_train$Item_Type, mean, na.rm = TRUE)
class(match_table)

NA_positions<- which(is.na(store_train$Item_Weight))
store_train$Item_Weight[NA_positions]<- match_table[store_train$Item_Type[NA_positions]]

#Outlet size null values
# 
# NROW(store_train[store_train$Outlet_Size== '',])
# 
# for (i in unique(store_train$Outlet_Type)) {
#  print(i) 
# print(summary(store_train$Outlet_Size[store_train$Outlet_Type == i]))
# }

#Outlet size cannot be filled appropriately leave it!
for (j in unique(store_train$Outlet_Location_Type)) {
  
for (i in unique(store_train$Outlet_Type)) {
  print(j) 
  print(i)
  print(summary(store_train$Outlet_Size[store_train$Outlet_Type == i &store_train$Outlet_Location_Type == j ]))
}
}


attach(store_train)
# getting the required columns

colnames(store_train)

#Replacing "" in outlet size to Na
store_train[Outlet_Size== "","Outlet_Size"]<- NA
colSums(is.na(store_train))


#########################Predicting missing outlet sizes using decision trees######################
colnames(store_train)
outlet_info<- store_train[!is.na(store_train$Outlet_Size), c(9,10,11)]
levels(outlet_info$Outlet_Size)
outlet_info$Outlet_Size<- as.character(outlet_info$Outlet_Size)
outlet_info$Outlet_Size<- as.factor(outlet_info$Outlet_Size)

str(outlet_info)


outlet_missing<-store_train[is.na(store_train$Outlet_Size), c(9,10,11,12)]
dim(outlet_missing) 
colnames(outlet_info)


library(rpart)
library(randomForest)
library(rpart.plot)
# 
# tree2<- rpart(Outlet_Size~., data = outlet_info, method = "class")
# rpart.plot::prp(tree2)
# pred<-predict(tree2, newdata = outlet_info, type = "class")
# 
# table(pred, outlet_info$Outlet_Size)


forest1<-randomForest(Outlet_Size~., outlet_info, ntree = 500)
plot(forest1)


pred<- predict(forest1, newdata = outlet_info, type = "class")
table(pred, outlet_info$Outlet_Size)

pred_miss<- predict(forest1, newdata = outlet_missing, type = "class")
summary(pred_miss)

#Filling missing outlet size with pred values
store_train[is.na(store_train$Outlet_Size),"Outlet_Size"]<- pred_miss
colnames(store_train)

#required columns
store_train1<- store_train[, -c(1,7)]

colSums(is.na(store_train1))





#######same prepossing for test data#####################################################

colnames(store_test)
head(store_test)
str(store_test)

#Checking null values
colSums(is.na(store_test))


# correcting ItemFat content column
store_test$Item_Fat_Content<- if_else(store_test$Item_Fat_Content %in% c('reg', 'Regular'), 'Regular', 'Low' )


#Filling missing weight with catogery mean
match_table<- tapply(store_test$Item_Weight, store_test$Item_Type, mean, na.rm = TRUE)
class(match_table)

NA_positions<- which(is.na(store_test$Item_Weight))
store_test$Item_Weight[NA_positions]<- match_table[store_test$Item_Type[NA_positions]]

####Filling missing outlet size on test set
attach(store_test)
store_test$Outlet_Size<- as.factor(store_test$Outlet_Size)
str(store_test)
summary(store_test)
store_test[Outlet_Size == "","Outlet_Size"]<- NA
colSums(is.na(store_test))
outlet_info<- store_test[!is.na(store_test$Outlet_Size), c(9,10,11)]
levels(outlet_info$Outlet_Size)
outlet_info$Outlet_Size<- as.character(outlet_info$Outlet_Size)
outlet_info$Outlet_Size<- as.factor(outlet_info$Outlet_Size)

str(outlet_info)

#predicting missing outlt size

outlet_missing<-store_test[is.na(store_test$Outlet_Size), c(9,10,11)]
dim(outlet_missing) 
colnames(outlet_info)

forest1<-randomForest(Outlet_Size~., outlet_info, ntree = 500)
plot(forest1)


pred<- predict(forest1, newdata = outlet_info, type = "class")
table(pred, outlet_info$Outlet_Size)

pred_miss<- predict(forest1, newdata = outlet_missing, type = "class")
summary(pred_miss)

#Filling missing outlet size with pred values
store_test[is.na(store_test$Outlet_Size),"Outlet_Size"]<- pred_miss
colnames(store_test)


################################################################################################
#Final Train test
head(store_train1)
colnames(store_train1)

#Final test set
colnames(store_test)
store_test1<- store_test[, -c(1,7)]
head(store_test1)
colnames(store_test1)

###############Feature engineering#################################

#getting age of store in years

store_train1$Store_Age<- year(today()) - store_train1$Outlet_Establishment_Year
store_test1$Store_Age <- year(today()) - store_test1$Outlet_Establishment_Year

# droping estabilishment year

store_train1<- store_train1[, -6]
store_test1 <- store_test1[,-6]


colSums(is.na(store_test1))
colSums(is.na(store_train1))

store_train1$Item_Fat_Content<- as.factor(store_train1$Item_Fat_Content)
store_test1$Item_Fat_Content<- as.factor(store_test1$Item_Fat_Content)

str(store_train1)
str(store_test1)
setwd("D:/Shashank R files/Ivy capstone project")

write.csv(store_train1, "Train_preprocessed.csv")
write.csv(store_test1, "Test_preprocessed.csv")

#Outlier detection

quantile(store_train1$Item_Outlet_Sales,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
summary(store_train1$Item_Outlet_Sales)

#removing outlliers
store_train1 <- store_train1[store_train1$Item_Outlet_Sales <= 8213,]

#Creating dummy variables
#install.packages("fastDummies", dependencies = TRUE)
library(fastDummies)

newtrain<- dummy_cols(store_train1,remove_first_dummy = TRUE)
write.csv(newtrain, "train_with dummies.csv")

newtest <- dummy_cols(store_test1, remove_first_dummy = TRUE)
write.csv(newtrain, "test_with dummies.csv")



#Final Trainset

head(newtrain)
summary(newtrain)
dim(newtrain)
#final test set
head(newtest)
summary(newtest)
dim(newtest)
####################Building the Model######################################
library(rpart)
library(rpart.plot)
library(randomForest)
colnames(newtrain)

colnames(newtrain)<-gsub(" ","_", colnames(newtrain))
tree1<- rpart(Item_Outlet_Sales~., data = newtrain)
prp(tree1)

pred_tree1<-predict(tree1, newdata = newtrain)

#install.packages("DescTools", dependencies = TRUE)
library(DescTools)
#Decision tree
RMSE_tree1<-RMSE(pred_tree1, newtrain$Item_Outlet_Sales)
MAPE_tree1<-MAPE(pred_tree1, newtrain$Item_Outlet_Sales)

plot(newtrain$Item_Outlet_Sales, pred_tree1,col='blue', pch=16, ylab = "predicted", xlab = "Actual")
abline(0,1)

#Random forest
set.seed(0)
colnames(newtrain)
spl <- sample.split(newtrain$Item_Outlet_Sales, 0.7)
forest_train<- newtrain[spl==TRUE,]
forest_test<- newtrain[spl==FALSE,]


forest1<- randomForest(formula = Item_Outlet_Sales~., data = forest_train , ntree =100)
print(forest1)
plot(forest1)
summary(forest1)
pred_forest1<- predict(forest1, newdata = forest_test)

MAPE_Forest1<-MAPE(pred_forest1, forest_test$Item_Outlet_Sales)
MAPE_Forest1
RMSE_Forest1<-RMSE(pred_forest1, forest_test$Item_Outlet_Sales)
RMSE_Forest1
plot(forest_test$Item_Outlet_Sales, pred_forest1,col='blue', pch=16, ylab = "predicted", xlab = "Actual")

abline(0,1)

#Getting variable importance in random forest
variableimportance<-as.data.frame(forest1$importance)
colnames(forest_train)
names<-rownames(variableimportance)
rownames(variableimportance)<- NULL

variableimportance<-cbind(names,variableimportance)
variableimportance<- variableimportance[order(-variableimportance$IncNodePurity),]
plot(variableimportance)

#Automating random forest for different no of features
RMSE_Forest1<- RMSE_Forest1[]
MAPE_Forest1<- MAPE_Forest1[]
plots<- plots[]
for (i in 1:(length(colnames(forest_train))-1)) {
i=6
important_cols<-as.integer(rownames(head(variableimportance,i)))
important_forest_train<- subset(forest_train[,-4], select = c(important_cols))
important_forest_train<- cbind(important_forest_train, forest_train[,4])

forest1<- randomForest(formula = Item_Outlet_Sales~., data = important_forest_train , ntree =100)
#print(forest1)
#plot(forest1)
#summary(forest1)
pred_forest1<- predict(forest1, newdata = forest_test)
MAPE<-MAPE(pred_forest1, forest_test$Item_Outlet_Sales)
RMSE<-RMSE(pred_forest1, forest_test$Item_Outlet_Sales)
plot(forest_test$Item_Outlet_Sales, pred_forest1,col='blue', pch=16, ylab = "predicted", xlab = "Actual", main = c("Actual vs predicted plot",i))
abline(0,1)
RMSE_Forest1[i]<- RMSE
MAPE_Forest1[i]<- MAPE
}


RMSE_Forest1
MAPE_Forest1
which.min(RMSE_Forest1)
which.min(MAPE_Forest1)


predictors<-print(paste(colnames(newtrain), sep = "+", collapse = '+'))

maxValue<- apply(newtrain, 2, max)
minValue<- apply(newtrain, 2,min)


newtrain_scaled<- as.data.frame(scale(newtrain, center = minValue, scale = maxValue - minValue))

#Splitting into train and test
set.seed(0)
spl<- sample.split(newtrain_scaled$Item_Outlet_Sales, SplitRatio = 0.7)

newtrain_train<- newtrain_scaled[spl==TRUE,]
newtrain_test<- newtrain_scaled[spl==FALSE,]


library(neuralnet)
set.seed(0)

n<- neuralnet(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_MRP+
                Store_Age+Item_Fat_Content_Regular+Item_Type_Soft_Drinks
              +Item_Type_Meat+Item_Type_Fruits_and_Vegetables+
                Item_Type_Household+Item_Type_Baking_Goods+
                Item_Type_Snack_Foods+Item_Type_Frozen_Foods+
                Item_Type_Breakfast+Item_Type_Health_and_Hygiene+
                Item_Type_Hard_Drinks+Item_Type_Canned+Item_Type_Breads+
                Item_Type_Starchy_Foods+Item_Type_Others+Item_Type_Seafood
              +Outlet_Size_High+Outlet_Size_Small+
                Outlet_Location_Type_Tier_3+Outlet_Location_Type_Tier_2+
                Outlet_Type_Supermarket_Type2+Outlet_Type_Grocery_Store+
                Outlet_Type_Supermarket_Type3, data = newtrain_train, hidden = c(2,2,1), linear.output = TRUE)

plot(n)

pred_nn<-compute(n, newtrain_train[,-4])
predictions<- pred_nn$net.result*(maxValue[4]- minValue[4]) + minValue[4]

colnames(newtrain)
pred_nn<-compute(n, newtrain_test[,-4])
predictions<- pred_nn$net.result*(maxValue[4]- minValue[4]) + minValue[4]

actual_test<-newtrain_test$Item_Outlet_Sales*(maxValue[4]- minValue[4]) + minValue[4]

MAPE(predictions, actual_test)
RMSE(predictions, actual_test)

plot(actual_test, predictions, col='blue', pch=16, ylab = "predicted", xlab = "Actual")
abline(0,1)


#Linear regression
Rsquares<- list()
for (i in 1:(length(colnames(forest_train))-1)) {
  important_cols<-as.integer(rownames(head(variableimportance,i)))
  important_forest_train<- subset(forest_train[,-4], select = c(important_cols))
  important_forest_train<- cbind(important_forest_train, forest_train[,4])

  fit<- lm(data = important_forest_train, formula = Item_Outlet_Sales~.)
a<-summary(fit)
 Rsquares[i]<-a$adj.r.squared
}
Rsquares

i=10
important_cols<-as.integer(rownames(head(variableimportance,i)))
important_forest_train<- subset(forest_train[,-4], select = c(important_cols))
important_forest_train<- cbind(important_forest_train, forest_train[,4])


attach(important_forest_train)
fit<- lm(data = important_forest_train, formula = Item_Outlet_Sales~.)
summary(fit)


fit2<-lm(data = important_forest_train, formula = Item_Outlet_Sales~Item_MRP+ 
           Outlet_Type_Grocery_Store+Item_Visibility+Store_Age
         +Outlet_Type_Supermarket_Type3+Item_Fat_Content_Regular+
           Outlet_Location_Type_Tier_3 )
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)

#multicolinearity check
vif(fit2)

#Autocorrelation check
durbinWatsonTest(fit2)

#Homoskedesticity check
bptest(fit2)

ncvTest(fit2)# Serious Hetroskedesticity found , HEll!

# library(caret)
# #getting boxCox transformation
# 
# BCMod<- BoxCoxTrans(important_forest_train$Item_Outlet_Sales)
# 
# predict(BCMod, important_forest_train$Item_Outlet_Sales)
# 
# important_forest_train<- cbind(important_forest_train, Item_Outlet_Sales_BCmod = predict(BCMod,important_forest_train$Item_Outlet_Sales))
# 
# fit<- lm(data = important_forest_train, formula = Item_Outlet_Sales_BCmod~.)
# summary(fit)
# 
# 
# fit2<-lm(data = important_forest_train, formula = Item_Outlet_Sales_BCmod~Item_MRP+ 
#            Outlet_Type_Grocery_Store+Store_Age
#          +Outlet_Type_Supermarket_Type3+Item_Fat_Content_Regular+Outlet_Location_Type_Tier_3 )
# 
# summary(fit2)
# 
# par(mfrow=c(2,2))
# plot(fit2)
# 
# #multicolinearity check
# vif(fit2)
# 
# #Autocorrelation check
# durbinWatsonTest(fit2)
# 
# #Homoskedesticity check
# bptest(fit2)
# 
# ncvTest(fit2)# Serious Hetroskedesticity found , HEll!
# 


###########################################################################
fit2<-lm(data = important_forest_train, formula = Item_Outlet_Sales~Item_MRP+ 
           Outlet_Type_Grocery_Store+Item_Visibility+Store_Age
         +Outlet_Type_Supermarket_Type3+Item_Fat_Content_Regular+
           Outlet_Location_Type_Tier_3 )



important_forest_train$resi <- fit2$residuals
colnames(important_forest_train)
varfunc <- lm(log(resi^2) ~ log(Item_MRP)+log(Store_Age), data = important_forest_train)
summary(varfunc)
important_forest_train$varfunc <- exp(varfunc$fitted.values)

fit3 <- lm(Item_Outlet_Sales~Item_MRP+ 
             Outlet_Type_Grocery_Store+Item_Visibility+Store_Age
           +Outlet_Type_Supermarket_Type3+Item_Fat_Content_Regular+
             Outlet_Location_Type_Tier_3  , weights = 1/sqrt(varfunc), data = important_forest_train)

summary(fit3)




par(mfrow=c(2,2))
plot(fit3)


#Homoskedesticity check
bptest(fit3)

ncvTest(fit3)# Serious Hetroskedesticity found , HEll!


