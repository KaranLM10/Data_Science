#Assignment Of Data Science & Analytics
#Student_Name : Karan Jadhav
#Student_ID : R00183239

#1.
library(readxl)
Training_Data <- read_excel("Data_Science_Analytics/Credit_Risk6_final.xlsx",sheet = "Training_Data")
View(Training_Data)

Scoring_Data <- read_excel("Data_Science_Analytics/Credit_Risk6_final.xlsx",sheet = "Scoring_Data")
View(Scoring_Data)

by(Training_Data$Employment,Training_Data$`Loan Reason`,summary)
#Identifying Loan Reasons

library(tidyverse)
boxplot(Training_Data$Age~Training_Data$`Loan Reason` ,col=c("grey","blue","yellow","orange","purple","pink","white","black","maroon")
        ,xlab = "Loan Reason" ,ylab = "Age")

boxplot(Training_Data$Age~Training_Data$`Credit Standing`,col=c("blue","orange")
        ,xlab = "Credit Standing" ,ylab = "Age")


str(Training_Data)

library(GGally)

ggpairs(Training_Data, mapping = aes(color=`Loan Reason`),columns = c(3,14), axisLabels = "show"
        ,title="Credit History vs Credit Standing",legend=c(2,2))

xtabs(~`Loan Reason`+`Credit Standing`,Training_Data)
plot(xtabs(~`Loan Reason`+`Credit Standing`,Training_Data),main="Job Type vs Loan Reason vs Credit History")


ggplot(Training_Data, aes(Training_Data$`Job Type`))+
  geom_density(fill="yellow")+facet_grid(. ~ `Credit Standing`)+labs(title="Density plot",x="Distribution Of Data", y="Job Type")


xtabs(~`Job Type`+`Loan Reason`+`Credit Standing`,Training_Data)
plot(xtabs(~`Job Type`+`Loan Reason`+`Credit Standing`,Training_Data),main="Job Type vs Loan Reason vs Credit Standing")


xtabs(~`Loan Reason`+`Job Type`+`Credit Standing`,Training_Data)
plot(xtabs(~`Loan Reason`+`Job Type`+`Credit Standing`,Training_Data),main="Job Type vs Loan Reason vs Credit History")



ggplot(Training_Data, aes(x=Training_Data$`Loan Reason`, y=Training_Data$`Job Type`, color=Training_Data$`Credit Standing`))+ #,shape=as.factor(CriminalRecord))) +
  geom_point()+labs(title="Scatter plot Of Loan Reason vs Job Type",x="Loan Reason", y="Job Type")


#2 Decision Tree
library(tidyverse)

Training_Data$`Checking Acct`=as.factor(Training_Data$`Checking Acct`)
Training_Data$`Credit History`=as.factor(Training_Data$`Credit History`)
Training_Data$`Loan Reason`=as.factor(Training_Data$`Loan Reason`)
Training_Data$`Savings Acct`=as.factor(Training_Data$`Savings Acct`)
Training_Data$Employment=as.factor(Training_Data$Employment)
Training_Data$`Personal Status`=as.factor(Training_Data$`Personal Status`)
Training_Data$Housing=as.factor(Training_Data$Housing)
Training_Data$`Job Type`=as.factor(Training_Data$`Job Type`)
Training_Data$`Foreign National`=as.factor(Training_Data$`Foreign National`)
Training_Data$`Credit Standing`=as.factor(Training_Data$`Credit Standing`)

names(Training_Data) = str_replace_all(names(Training_Data), c(" " = "."))
names(Scoring_Data) = str_replace_all(names(Scoring_Data), c(" " = "."))
names(Training_Data)=gsub('[//(//)]','',names(Training_Data))


View(Training_Data)

library(rpart)
library(caTools)
set.seed(239)
pd <- sample(2,nrow(Training_Data),replace = TRUE,prob = c(0.8,0.2))
train <- Training_Data[pd==1,]
validate <- Training_Data[pd==2,]

library(mice)
p <- function(x){sum(is.na(x))/length(x)*1}
apply(train,2,p)
md.pattern(train)
md.pairs(train)
library(VIM)

impute1 <- mice(train[2:14],m=3, seed=239)
print(impute1)
impute1$imp$Housing

train<-mice::complete(impute1,2)

View(train)


apply(validate,2,p)
md.pattern(validate)

impute2 <- mice(validate[2:14],m=3,seed=239)
print(impute2)

validate<-mice::complete(impute2,2)

View(train)
View(validate)
library(rpart)
library(rpart.plot)
library(tree)
model_tree1 <- rpart(Credit.Standing~ .,data=train, method="class",minbucket=100)

rpart.plot(model_tree1)

#3


library(caret)
library(e1071)
pred_tree <-predict(model_tree1,newdata =validate,type = "class") 

table_check <- table(Predicted = pred_tree, Actual=validate$Credit.Standing) 


View(table_check)
confusionMatrix(table_check) #71.65%


#unique(Training_Data$Age)

#4 

library(randomForest)

rf <- randomForest(Credit.Standing~.,data=train)
print(rf)
attributes(rf)

rf$confusion


#Prediction & Confusion Matrix - train data
library(caret)

pred_rf <-predict(rf,train) 
confusionMatrix(pred_rf,train$Credit.Standing)


#Prediction & Confusion Matrix - test data
pred_rf_test <- predict(rf,validate)
confusionMatrix(pred_rf_test,validate$Credit.Standing)


#Error Rate Of Random Forest
plot(rf)


#Tune mtry for bagging
t <- tuneRF(train[,-13],train[,13],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            trace = TRUE,
            improve = 0.05)


#to increase accuracy by bagging & boosting
rf_new <- randomForest(Credit.Standing~.,data=train,
                       ntree=200,
                       mtry=7,
                       importance=TRUE,
                       proximity=TRUE)

print(rf_new)
print(rf)


pred_rf_new <-predict(rf_new,validate) 

confusionMatrix(pred_rf_new,validate$Credit.Standing) #New Accuracy :  75.59%
confusionMatrix(pred_rf_test,validate$Credit.Standing) #Old Accuracy : 74.02%

#Number Of Trees
hist(treesize(rf_new),
     main = "No Of Nodes for the Trees",
     col = "red")


#Variable Importance
varImpPlot(rf_new,
           sort = TRUE,
           n.var = 10,
           main = "Top 10 Variable Importance")
importance(rf_new)
varUsed(rf_new)

#Extract Single Tree
getTree(rf_new,1,labelVar = TRUE)

#Mutli Dimensional Matrix
MDSplot(rf_new,train$Credit.Standing)

#5 Incorrect Pattern

Training_Data_test <- read_excel("Data_Science_Analytics/Credit_Risk6_final.xlsx",sheet = "Training_Data")
Training_Data_test <- na.omit(Training_Data_test)
View(Training_Data_test)

Training_Data_test$`Checking Acct`=as.factor(Training_Data_test$`Checking Acct`)
Training_Data_test$`Credit History`=as.factor(Training_Data_test$`Credit History`)
Training_Data_test$`Loan Reason`=as.factor(Training_Data_test$`Loan Reason`)
Training_Data_test$`Savings Acct`=as.factor(Training_Data_test$`Savings Acct`)
Training_Data_test$Employment=as.factor(Training_Data_test$Employment)
Training_Data_test$`Personal Status`=as.factor(Training_Data_test$`Personal Status`)
Training_Data_test$Housing=as.factor(Training_Data_test$Housing)
Training_Data_test$`Job Type`=as.factor(Training_Data_test$`Job Type`)
Training_Data_test$`Foreign National`=as.factor(Training_Data_test$`Foreign National`)
Training_Data_test$`Credit Standing`=as.factor(Training_Data_test$`Credit Standing`)

names(Training_Data_test) = str_replace_all(names(Training_Data_test), c(" " = "."))
names(Training_Data_test)=gsub('[//(//)]','',names(Training_Data_test))

pd_check <- sample(2,nrow(Training_Data_test),replace = TRUE,prob = c(0.8,0.2))
train_check <- Training_Data_test[pd_check==1,]
validate_check <- Training_Data_test[pd_check==2,]
model_tree2 <- rpart(Credit.Standing~ .,data=train_check, method="class",minbucket=25)


pd_train=predict(model_tree2, newdata = Training_Data_test,type = "class")
predictions1=cbind(as.data.frame(pd_train), as.data.frame(Training_Data_test$Credit.Standing))
predictions1=cbind(Training_Data_test$ID,predictions1)
View(predictions1)
colnames(predictions1)=c("ID_No","Predicted_Values","Actual_Values")
check_pattern=filter(predictions1,Predicted_Values!=Actual_Values)

View(check_pattern)



#6 Entropy & Info Gain

# Imputing data for info gain

library(mice)
p <- function(x){sum(is.na(x))/length(x)*1}
apply(Training_Data,2,p)
md.pattern(Training_Data)
md.pairs(Training_Data)
library(VIM)
library(tidyr)
library(dplyr)
impute3 <- mice(Training_Data[2:14],m=3, seed=239)
print(impute3)

str(Training_Data)
Training_Data<-mice::complete(impute3,2)
View(Training_Data)

table(Training_Data$Credit.Standing)
Standing_check <- prop.table(table(Training_Data$Credit.Standing))

str(Training_Data)

#Creating function to check values against the Credit.Standing column
func_check <- function(x) {table(Training_Data[,x],Training_Data[,13])}

print(func_check(5))  #Considering the employment column


# Checking Row wise proportion of Employment column in terms of Credit.Standing column
#Here margin=1,indicates we are considering proportion row wise
func_check1 <- function(x) {prop.table(table(Training_Data[,x],Training_Data[,13]), margin = 1)}  
print(func_check1(5))  


#Calculating Entropy with the formula
-func_check1(5)*log2(func_check1(5))
 
#In case, where the value are zero, we should use Laplace smoothing, giving +1e-6
func_check2 <- function(x) {prop.table(table(Training_Data[,x],Training_Data[,13]) + 1e-6, margin = 1)}  
print(func_check2(5)) 

#Checking rowsum for the entire row
rowSums(-func_check2(5)*log2(func_check2(5)))
prop.table(table(Training_Data$Employment))

#Let's sum the proportion & multiply with the rowsums of each row.
sum(prop.table(table(Training_Data$Employment))*rowSums(-func_check2(5)*log2(func_check2(5))))


Standing_check <- prop.table(table(Training_Data$Credit.Standing))
(entropy_total <-sum(-Standing_check*log2(Standing_check)))

#Here we are combining all the functions used above
entropy_tab <- function(x){ 
  func_check2 <- prop.table(table(Training_Data[,x],Training_Data[,13]),margin = 1)
  
  sum(prop.table(table(Training_Data[,x]))*rowSums(-func_check2*log2(func_check2)))}

colnames(Training_Data)

#Filtering data from Checking.Acct - Age
col_names=colnames(Training_Data)[1:12]

#str(Training_Data)
#Training_Data$Months.since.Checking.Acct.opened=as.factor(Training_Data$Months.since.Checking.Acct.opened)
#Training_Data$Residence.Time.In.current.district=as.factor(Training_Data$Residence.Time.In.current.district)
#Training_Data$Age=as.factor(Training_Data$Age)

#Here we are finding the minimum entropy value.We can then verify why our decision tree was split based on a certain label.
Min_IG <- sort(sapply(col_names,entropy_tab),decreasing = FALSE)[1]
Min_IG



#Finding Information Gain, we substract the entropy total.
Information_tab <- function(x){ 
  func_check2 <- prop.table(table(Training_Data[,x],Training_Data[,13]),margin = 1)
  entropy_total-sum(prop.table(table(Training_Data[,x]))*rowSums(-func_check2*log2(func_check2)))}

View(col_names)

#Finding the maximum information gain used to split the decision tree
Max_IG <- sort(sapply(col_names,Information_tab),decreasing = TRUE)[1]
Max_IG

#7 Boosting using adabag

ID <- seq(1,10,1)
Label=c(0,1,1,0,1,1,0,1,0,0)
P1=c(1,1,0,1,0,0,1,0,0,1)
Weight_check=rep(0.1,10)
Predict_val=sample(0:1,size = 10,replace = TRUE)

adaboost_list <- vector("list",1)

#= adaboost_list[[i-1]]$newweight                  
for(i in 1:4)
{
  set.seed(239)
  if(i==1)
  {
    adaboost_list[[i]] = data.frame(ID,Label,
                                    P1 ,Weight_check,
                                    Predict_val)
  }
  else
  {
    adaboost_list[[i]] = data.frame(ID,Label,P1,
                                    Weight_check,Predict_val)
  }
  
  adaboost_list[[i]] = cbind(adaboost_list[[i]] , error =ifelse(adaboost_list[[i]]$Predict_val ==adaboost_list[[i]]$Label,0,1))
  adaboost_list[[i]] = cbind(adaboost_list[[i]] , errorweight =adaboost_list[[i]]$error*adaboost_list[[i]]$Weight_check)
  
  errorsum = sum(adaboost_list[[i]]$errorweight)
  print(errorsum)
  alpha = 0.5*log((1-errorsum)/errorsum)
  incorrect = exp(-(alpha)*(-1)) #adjusting probability for incorrect values
  correct = exp(-(alpha)*(1)) #adjusting probability for correct values
}

adaboost_list
 
#8 ROC CURVE
Training_Data$Credit.Standing=as.character(Training_Data$Credit.Standing)

Training_Data$Credit.Standing <- replace(Training_Data$Credit.Standing,Training_Data$Credit.Standing=="Good",1)
Training_Data$Credit.Standing <- replace(Training_Data$Credit.Standing,Training_Data$Credit.Standing=="Bad",0)

Training_Data$Credit.Standing=as.numeric(Training_Data$Credit.Standing)

library(randomForest)
model_rf1 <- randomForest(Credit.Standing~ Checking.Acct +Credit.History +Loan.Reason +Savings.Acct+ Employment + Personal.Status +Housing +Job.Type +Foreign.National, data=Training_Data, nodesize=10, ntree=1000, type="prob")

#ROC
true_pos_rate=character()
false_pos_rate=character()


for (i in seq(0,1,0.01) ){
  print(i)
  true_pos_rate=c(true_pos_rate,sum(model_rf1$predicted>=i & Training_Data$Credit.Standing ==1)/length(Training_Data$Credit.Standing==1))
  
  false_pos_rate=c(false_pos_rate,sum(model_rf1$predicted>=i & Training_Data$Credit.Standing ==0)/length(Training_Data$Credit.Standing==0))
}

plot(false_pos_rate,true_pos_rate)


#To verify the curve using the package
Ptest1 <- predict(model_rf1,newdata=Training_Data,type="response")

#using library to verify whether the curve is right or not?
library(pROC) 
roc_check=roc(Training_Data$Credit.Standing~Ptest1) 
plot(roc_check)
