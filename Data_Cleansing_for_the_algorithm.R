#Assignment Of Intro to R
#Student_Name : Karan Jadhav
#Student_ID : R00183239

#1. Reading data using readxl package.
library(readxl)
Sheet1 <- read_excel("R_language/assignment1.xlsx",sheet="Sheet1")
View(Sheet1)

Sheet2 <- read_excel("R_language/assignment1.xlsx",sheet="Sheet2")
View(Sheet2)


#2.Generating a data frame from each sheet in a file
Data_test1 <- data.frame(Sheet1)

Data_test2 <- data.frame(Sheet2)


#3.Regenearting a new identification number for each subject

Identifier <- seq(1,160,1)
#Creating sequential numbers from 1-160 based on the total dataset


F_Data <- cbind(Identifier,Data_test1)       
#Using c bind function, to append a column into existing data frame.


View(F_Data)                                    


#4.Additional Identifiers from FirstName & Surname

#a <- substr(F_Data$FirstName,1,1)        
#b <- substr(F_Data$Surname,1,2)         
#checking output seperately to check that the derised result is obtained.       

Add_Identifier <- paste(substring(F_Data$Identifier,1),substring(F_Data$FirstName,1,1),substring(F_Data$Surname,2,2))
#The Result Of the 3 different substrings are combined to form one string using paste function

Data_Sheet1 <- cbind(Add_Identifier,F_Data) #binding data into existing data frame
View(Data_Sheet1)

View(Data_Sheet1)
View(Data_test2)

#5 Merging Health Data from Data_test2(Sheet2) to Data_Sheet1(Sheet1) 

#Note : Data_Sheet1 is the modified data from Sheet1.(Modifications include adding Identifier)
Data_Sheet1$Health <- Data_test2$Health[match(Data_Sheet1$IDNumber,Data_test2$IDNumber)]

#Finding Matching rows ,for those which are non matching it will return NA Values


#Merged_Data1 <- merge(Data_Sheet1,Sheet2, by=('IDNumber'))

View(Data_Sheet1)

library(tidyverse)      #to include tidyverse library
#Pre-Processing for Task 6
Sheet2_filtered <- filter(Data_test2,is.na(Data_test2$IDNumber))
#Retrieving values those have NA in IDNumber
View(Sheet2_filtered)


Sheet2_filtered$IDNumber <- NULL   
#to remove column from the data frame

View(Sheet2_filtered)

#6 Match data based using FirstName & Surname
Merged_Data1 <- Data_Sheet1%>%
  full_join (Sheet2_filtered, by=c('FirstName','Surname'))
#used full join function to get all the columns from Data_Sheet1(Sheet1) & all columns from Sheet2_filtered(Sheet2)

View(Merged_Data1)

#Merged_Data2 data frame to store combined Result from Health.x & Health.y into Health.x
Merged_Data2 <- data.frame(within(Merged_Data1, Health.x[is.na(Health.x)] <- Health.y[is.na(Health.x)]))
View(Merged_Data2)

#Removing Extra column
Merged_Data2$Health.y <- NULL

#Renaming Health.x to Health
Output_Data <- Merged_Data2%>%
  rename(
    Health=Health.x
    )

View(Output_Data)

#7 Add a Column for Age Range

Output_Data$Age_Name_Cat[Output_Data$Age >0 & Output_Data$Age <18]='1'
Output_Data$Age_Name_Cat[Output_Data$Age >=18 & Output_Data$Age <35]='2'
Output_Data$Age_Name_Cat[Output_Data$Age >=35 & Output_Data$Age <54]='3'
Output_Data$Age_Name_Cat[Output_Data$Age >=54 & Output_Data$Age <74]='4'
Output_Data$Age_Name_Cat[Output_Data$Age >=74 ]='5'
#Created a Age Group named "Age_Name_Cat"


View(Output_Data)

summary(Output_Data)

#### 8 Filter data by each Age Category & Generate ggplot2 for the criminal Record ####
library(ggplot2)

#Filtering the data on the basis of Age Category
Age1 <- filter(Output_Data, Output_Data$Age_Name_Cat=="1")
Age2 <- filter(Output_Data, Output_Data$Age_Name_Cat=="2")
Age3 <- filter(Output_Data, Output_Data$Age_Name_Cat=="3")
Age4 <- filter(Output_Data, Output_Data$Age_Name_Cat=="4")
Age5 <- filter(Output_Data, Output_Data$Age_Name_Cat=="5")
View(Age5)



#Bar Plot for number of Criminal records against Each Age Category
ggplot(Output_Data, 
       mapping=aes(x=Output_Data$Age_Name_Cat,fill=Output_Data$CriminalRecord))+geom_bar()+labs(title="Bar Plot Of Criminal Record vs Age Category",x="Age Category Name", y="Number Of Occurences")+theme_bw()

#Bifurcating Number Of Criminal Record in each Age Category
ggplot(data=Output_Data)+geom_bar(aes(x=Age_Name_Cat,fill=as.factor(CriminalRecord)))+labs(title="Bar Plot Of Age Category vs No. Of Occurences",x="Age Category Name", y="Number Of Occurences")+theme_bw()


#### 9 Relationships between Weight, Height, Age,Criminal Record ####


ggplot(Output_Data, aes(x=Output_Data$Weight.kg., y=Output_Data$Height.m., color=Age_Name_Cat ,shape=as.factor(CriminalRecord))) +
  geom_point()+labs(title="Plot Of Weight vs Height",x="Weight", y="Height")
#No evident Observations across the data

ggplot(Output_Data,aes(x=Age,y=Weight.kg.,fill=Age_Name_Cat))+geom_violin()+labs(title="Plot Of Age vs Weight",x="Age", y="Weight")
#For all Observations, we have more population which lies in Weight Range 100-150

ggplot(Output_Data,aes(x=Age,y=Height.m.,fill=Age_Name_Cat))+geom_violin()+labs(title="Plot Of Age vs Height",x="Age", y="Height")
#Observations Age category 2, more population lies in the height range 1.8-1.9 
#Observations Age Category 5, more population lies in the height range 1.8-2.2

ggplot(Output_Data,aes(x=Age,y=Height.m.,color=Age_Name_Cat))+geom_point()+labs(title="Plot Of Age vs Height",x="Age", y="Height")

ggplot(data = Output_Data, mapping = aes(x=Output_Data$Weight.kg., y=Output_Data$Height.m., color = Age_Name_Cat))+
  geom_boxplot()+labs(title="Plot Of Weight vs Height",x="Weight", y="Height")
  geom_jitter(alpha = 0.9, color = "tomato")
#Age Category 3 looks symmetric but we need to verify it further.


Weight_check <- filter(Output_Data, Output_Data$Weight.kg.>="75" & Output_Data$Weight.kg.<="99" & Output_Data$Height.m.>="1.7")
#Filtering Data for Category 3

View(Weight_check)
hist(Weight_check$Height.m.,freq = FALSE,breaks=15)     #checking data using histogram

ggplot(Weight_check, aes(x=Weight.kg.))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed") 


Weight_check2 <- filter(Output_Data, Output_Data$Weight.kg.>="100" & Output_Data$Weight.kg.<="140" & Output_Data$Height.m.>="1.8")
#filtering data

View(Weight_check2)
hist(Weight_check2$Height.m.,freq = FALSE, breaks = 15)


#### 10 Relationship Between Height, Weight & Criminal Record
ggplot(data=Output_Data)+aes(Output_Data$Weight.kg.,Output_Data$Height.m.,color=as.factor(CriminalRecord))+geom_point()+ 
  geom_smooth(method="lm",se=FALSE)+labs(title="Plot Of Weight vs Height",x="Weight", y="Height")



#Filtering for total number Of Criminal Records
Record_Check1 <- filter(Output_Data, Output_Data$CriminalRecord=="1")
View(Record_Check1)  #78 Records for Criminal Record 1

Record_Check2 <- filter(Output_Data, Output_Data$CriminalRecord=="2")
View(Record_Check2)  #82 Records for Criminal Record 2

#Checking Data Spread across the population
ggplot(data = Output_Data, mapping = aes(x=Output_Data$CriminalRecord, y=Output_Data$Weight.kg.))+
  geom_boxplot()+labs(title="Plot Of Weight vs Height",x="Weight", y="Height")
geom_jitter(alpha = 0.9, color = "tomato")
#For Criminal Record 1, data seems to be left skewed.
#For Criminal Record 2,data seems to be symmetric.
#Lets verify it further.


Record_Check3 <- filter(Output_Data, Output_Data$CriminalRecord=="2" & Output_Data$Weight.kg.>="0" & Output_Data$Weight.kg.<="150")
#filtering data

View(Record_Check3)

hist(Output_Data$Height.m.,freq = FALSE, breaks = 50)

hist(Output_Data$Weight.kg.,freq = FALSE, breaks = 50)

#### 11.

Final_Data <- filter(Output_Data,is.na(Output_Data$Health)==FALSE)  #filtering data taking values those are not na
View(Final_Data)

Final_Data$Health <- replace(Final_Data$Health,Final_Data$Health==1,0)

Final_Data$Health <- replace(Final_Data$Health,Final_Data$Health==2,1)
#replace function is used for replacing existing values to new values
View(Final_Data)




library(caTools)
#use caTools function to split, SplitRatio for 80%:20% splitting
input= sample.split(Final_Data,SplitRatio = 0.8)
#subsetting into Train data
train =subset(Final_Data,input==TRUE)
#subsetting into Test data
test =subset(Final_Data,input==FALSE)

logic_reg1 <- glm(Health ~ Weight.kg. + Height.m.+ Age, data = train, family = binomial)
#creating logistic regression based on categorical data i.e health & checking it on train data

P1 <- predict(logic_reg1,newdata=test,type="response")
#Predicting model on test data & assigning the result to P1.
head(P1) #Check values in P1

pred1 <- ifelse(P1>0.5, 1 , 0)          #Values can be either 0 or 1 in logistic regression so providing the conditions,if value>0.5==1 else 0
tab1 <- table(Predicted = pred1, Actual=test$Health)   #Checking the predicted values against the actual values.
View(tab1)
##         Actual
##Predicted  0  1
##        0 10  3
##        1  6 11

#(True Positive + True Negative) /( True Positive + True Negative + False Positive + False Negative)
#Model Accuracy is 70%

library(pROC)   #importing pROC library
log_roc=roc(test$Health~P1)   #using ROC function to check Actual values against Predicted values
plot(log_roc)       #ROC curve plot