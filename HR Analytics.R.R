#
# HR Analytics Case Study Group Submission by:
# 1. Sudeep Upadhya
# 2. Rishabh
# 3. Vikash Prasad
# 4. Amit Mankikar
#
# Date: 20-08-2017
#

#install.packages("caTools")
#install.packages("MASS")
#install.packages("car")
#install.packages("GGally")
#install.packages("e1071")
#install.packages("caret")
#install.packages("ROCR")
#install.packages("dplyr")
#install.packages("plotrix")

library(caret)
library(caTools)
library(MASS)
library(car)
library(GGally)
library(ROCR)
library(dplyr)
library(e1071)
library(plotrix)
library(ggplot2)

#
# Function that reads the in_time and out_time csv files
# It calculates the average working hours for every employee,
# merges into the master employee df and returns
#
getTimeSheetData <- function(employeeDF){
  
  in_time<- read.csv("in_time.csv", stringsAsFactors = F)
  in_time_stack <- stack(in_time[,-1])
  in_time_stack <- cbind(in_time_stack, in_time[,1])
  names(in_time_stack)<-c("In_Time", "Date", "EmployeeID")
  
  out_time<- read.csv("out_time.csv", stringsAsFactors = F)
  out_time_stack <- stack(out_time[,-1])
  out_time_stack <- cbind(out_time_stack, out_time[,1])
  names(out_time_stack)<-c("Out_Time", "Date", "EmployeeID")
  
  setdiff(in_time_stack$EmployeeID,out_time_stack$EmployeeID)
  setdiff(in_time_stack$Date,out_time_stack$Date)
  
  timeCalc<- merge(in_time_stack,out_time_stack, by=c("EmployeeID", "Date"), all = F)
  
  timeCalc$In_Time <- as.POSIXct(gsub("-", "/", timeCalc$In_Time), format = "%Y/%m/%d %H:%M:%S")
  timeCalc$Out_Time <- as.POSIXct(gsub("-", "/", timeCalc$Out_Time), format = "%Y/%m/%d %H:%M:%S")
  
  timeCalc$workingHours <- timeCalc$Out_Time - timeCalc$In_Time
  
  avgWorkHours <- aggregate(as.numeric(workingHours)~EmployeeID, timeCalc, mean)
  names(avgWorkHours)<-c("EmployeeID", "AvgWorkingHours")
  
  employeeDF <- merge(employeeDF, avgWorkHours, by = "EmployeeID")
  
  #debug only
  #write.csv(avgWorkHours, file = "avgWorkHours.csv")
  
  return(employeeDF)
}


#
# Set the working directory 
#
setwd("C:\\upgrade training\\Group Case Study 3")

#
# Read all the data files one after the other
#
Attrition<- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)

str(Attrition)
str(employee_survey)# verified that most columns are integer
str(manager_survey)

#
# Let us check if all records are unique and no
# of records is same for error free merge
#
t <- unique(Attrition$EmployeeID)#4410 entries
t <- unique(employee_survey$EmployeeID)#4410 entries
t <- unique(manager_survey$EmployeeID)#4410 entries

setdiff(Attrition$EmployeeID,employee_survey$EmployeeID) # Identical customerID across these datasets
setdiff(Attrition$EmployeeID,manager_survey$EmployeeID) # Identical customerID across these datasets

#
# Merge into the General dataframe: This is our master data frame
#
General<- merge(Attrition,employee_survey, by="EmployeeID", all = F)
General<- merge(General,manager_survey, by="EmployeeID", all = F)

#
# Observe and verify the data
#
str(General) # 4410 observation with 29 variables

#
# Add average working time 
#
General <- getTimeSheetData(General)

#
# Get rid of all columns having same value
# Ref: https://stackoverflow.com/questions/30544282/how-to-remove-columns-with-same-value-in-r
#
General <- General[vapply(General, function(x) length(unique(x)) > 1, logical(1L))]

#
# Verify if average working hours per employee has been correctly computed 
#
View(General)

#####
# After refering to Data Dictionary and EDA, we have concluded that the following variables are nominal:
# EmployeeID, Age, DistanceFromHome, JobLevel, MonthlyIncome, NumberOfCompaniesWorked, PercentSalaryHike,
# StockOptionLevel, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion,
# YearsWithCurrentManager, 
#
# The following variables are categorical:
# BusinessTravel, Department, Education, EducationField, Gender, JobRole, MaritalStatus, EnvironmentSatisfaction,
# JobSatisfaction, WorkLifeBalance, JobInvolvement, PerformanceRating
#
# The dependent variable is: Attrition
#
#####

#
# Finding the missing value & imputation
#
sum(is.na(General))# 111 na are there
sapply(General, function(x) sum(is.na(x)))
General<- General[!is.na(General$EnvironmentSatisfaction),]
General<- General[!is.na(General$JobSatisfaction),]
General<- General[!is.na(General$WorkLifeBalance),]

#
#replace N/A value to 0 for NumCompaniesWorked & TotalWorkingYears
#
General$NumCompaniesWorked[is.na(General$NumCompaniesWorked)==T] <-0
General$TotalWorkingYears[is.na(General$TotalWorkingYears)==T] <-0
sum(is.na(General))# no further na values avaialable

#
#Outlier treatment for all numeric vars
#
boxplot(General$Age)
quantile(General$Age,seq(0,1,0.01))# no outlier

boxplot(General$DistanceFromHome)
quantile(General$DistanceFromHome,seq(0,1,0.01))#no outlier

boxplot(General$JobLevel)
quantile(General$JobLevel,seq(0,1,0.01))#no outlier

boxplot(General$MonthlyIncome) # outliers exist
quantile(General$MonthlyIncome,seq(0,1,0.01))# outliers exist
summary(General$MonthlyIncome)

box <- boxplot.stats(General$MonthlyIncome, coef = 2)
out <- box$out
General1 <- General[ !General$MonthlyIncome %in% out, ]
General <- General1
boxplot(General$MonthlyIncome) # ~ 100 outliers removed
summary(General$MonthlyIncome)

boxplot(General$NumCompaniesWorked) # No outliers exist
quantile(General$NumCompaniesWorked,seq(0,1,0.01))

boxplot(General$PercentSalaryHike)
quantile(General$PercentSalaryHike,seq(0,1,0.01))#no outliers

boxplot(General$StockOptionLevel)
quantile(General$StockOptionLevel,seq(0,1,0.01))#no outliers

boxplot(General$TotalWorkingYears)
quantile(General$TotalWorkingYears,seq(0,1,0.01))# outliers exist
summary(General$TotalWorkingYears)

box <- boxplot.stats(General$TotalWorkingYears, coef = 2)
out <- box$out
General1 <- General[ !General$TotalWorkingYears %in% out, ]
General <- General1
boxplot(General$TotalWorkingYears) # ~ 100 outliers removed
summary(General$TotalWorkingYears)

boxplot(General$TrainingTimesLastYear)
quantile(General$TrainingTimesLastYear,seq(0,1,0.01))# very few outliers exist

boxplot(General$YearsAtCompany)
quantile(General$YearsAtCompany,seq(0,1,0.01))# outliers exist
summary(General$YearsAtCompany)

box <- boxplot.stats(General$YearsAtCompany, coef = 2)
out <- box$out
General1 <- General[ !General$YearsAtCompany %in% out, ]
General <- General1
boxplot(General$YearsAtCompany)
summary(General$YearsAtCompany)

boxplot(General$YearsSinceLastPromotion)
quantile(General$YearsSinceLastPromotion,seq(0,1,0.01)) # outliers exist
summary(General$YearsSinceLastPromotion)

box <- boxplot.stats(General$YearsSinceLastPromotion, coef = 2.5)
out <- box$out
General1 <- General[ !General$YearsSinceLastPromotion %in% out, ]
General <- General1
boxplot(General$YearsSinceLastPromotion) # ~150 outliers removed
summary(General$YearsSinceLastPromotion)

boxplot(General$YearsWithCurrManager)
quantile(General$YearsWithCurrManager,seq(0,1,0.01)) # No outliers exist

boxplot(General$AvgWorkingHours)
quantile(General$AvgWorkingHours,seq(0,1,0.01)) # No outliers exist



#
# EDA: uni-variate & bi-variate analysis
#
ggplot(General, 
       aes(x = General$Age, y = General$Education, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
## It appears many employees who quit are having below Master's education.
## They could be leaving for getting higher education

ggplot(General, 
       aes(x = General$TotalWorkingYears, y = General$JobRole, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
## It is evident - Not many employees leave if they have more than 10 years of experience

ggplot(General, 
       aes(x = General$AvgWorkingHours, y = General$Gender, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
## It is evident - higher the working hours, higher the attrition

ggplot(General, 
       aes(x = General$MonthlyIncome, y = General$Gender, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
## Lower monthly income are leaving

ggplot(General, 
       aes(x = General$EnvironmentSatisfaction, y = General$JobSatisfaction, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
## Clearly evident that low job satisfaction and environment satisfaction is high attrition

ggplot(General, 
       aes(x = General$Age, y = General$Gender, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()
## Attrition seems to subside over time. Larger Age, Lesser Attrition

#Bi-variate to check if any correlation between BusinessTravel, Department with Attrition
ggplot(General, 
       aes(x = General$Department, y = General$BusinessTravel, color = as.factor(General$Attrition)
       )) + geom_point( alpha = 0.2 ) + geom_jitter()

#Bi-variate to check if there is any correlation between department & Attrition
ggplot(General, aes(x=General$Department, y = (..count..)/sum(..count..), fill=factor(General$Attrition))) + geom_bar(position="fill") + scale_y_continuous(labels = "percent") + xlab("Department") + 
  ylab("Attrition  Percentage")+ ggtitle("Status of Attrition on Department")
  + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 

#Bi-variate to check if there is any correlation between Marital Status, Gender & Attrition
ggplot(General, 
       aes(x = General$MaritalStatus, y = General$Gender, color = as.factor(General$Attrition)
           )) + geom_point( alpha = 0.2 ) + geom_jitter()

#Uni-variate to check if there is any correlation between Gender & Attrition
p1 <-ggplot(General[General$Gender == "Female",], aes(x=MaritalStatus, y = (..count..)/sum(..count..), 
                      fill=factor(Attrition))) + 
  geom_bar(position="fill") + scale_y_continuous(labels = "percent") +
  xlab("Female Attrition") + 
  ylab("Percentage")+ 
  ggtitle("Status of Female Attrition on the Basis of the Marital Status") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 
p2 <-ggplot(General[General$Gender != "Female",], aes(x=MaritalStatus, y = (..count..)/sum(..count..), 
                                                      fill=factor(Attrition))) + 
  geom_bar(position="fill") + scale_y_continuous(labels = "percent") +
  xlab("Male Attrition") + 
  ylab("Percentage")+ 
  ggtitle("Status of Male Attrition on the Basis of the Marital Status") + 
  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.9), vjust=-0.25) 

grid.arrange(p1, p2)
rm(p1)
rm(p2)

#
# Standardizing all continuous variables 
#
General$Age<- scale(General$Age) 
General$DistanceFromHome<-scale(General$DistanceFromHome)
General$JobLevel<-scale(General$JobLevel)
General$MonthlyIncome<-scale(General$MonthlyIncome)
General$NumCompaniesWorked<-scale(General$NumCompaniesWorked)
General$PercentSalaryHike<-scale(General$PercentSalaryHike)
General$StockOptionLevel<-scale(General$StockOptionLevel)
General$TotalWorkingYears<-scale(General$TotalWorkingYears)
General$TrainingTimesLastYear<-scale(General$TrainingTimesLastYear)
General$YearsAtCompany<-scale(General$YearsAtCompany)
General$YearsSinceLastPromotion<-scale(General$YearsSinceLastPromotion)
General$YearsWithCurrManager<-scale(General$YearsWithCurrManager)
General$AvgWorkingHours<-scale(General$AvgWorkingHours)


#
# converting target variable Attrition from No/Yes character to levels 0/1 
#
General$Attrition<- ifelse(General$Attrition=="Yes",1,0)

#
#Attrition Rate
#
AttritionRate <- sum(General$Attrition)/nrow(General) # 16.82% Attrition-Rate

#
# Review the master df once after standardization 
# and before we do dummy variable creation for categorical variables
#
str(General)

#
# creating a dataframe of categorical variables
#
General_Cat<- General[,c(4, 5, 7, 8, 9, 11, 12, 22, 23, 24, 25, 26)]

#
# converting categorical variables to factor
#
General_fact<- data.frame(sapply(General_Cat, function(x) factor(x)))
str(General_fact)

#
# creating dummy variables for all factor attributes
#
dummies<- data.frame(sapply(General_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =General_fact))[,-1]))

#
# Create the final master file based on dummy value column join
#
General_Final<-cbind(General[,-c(4, 5, 7, 8, 9, 11, 12, 22, 23, 24, 25, 26)],dummies)

#
# Verify the number of observations and variables
#
str(General_Final)# all are in int/num. 3847 obs of 52 variables

####################################################################

#
# To convert the data in Test and Train
# We create a 30:70 spit in Test and Train 
#
set.seed(100)
indices = sample.split(General_Final$Attrition, SplitRatio = 0.7)
train = General_Final[indices,]
test = General_Final[!(indices),]

########################################################################
# Logistic Regression: Dependent variable is Attrition

#Initial model
model_1 = glm(Attrition ~ ., data = train [,-1],family = "binomial")# removed EmployeeID
summary(model_1) #AIC 1868.2

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2) #AIC: 1839.1
vif(model_2)

#
# Let us now undertake the task of model variable reduction for optimal
# predictor variable inclusion
#

#Let us exclude  EducationField.xMedical as its p-value is too high (0.126328)
model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + EducationField.xOther + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
              data = train[, -1])

summary(model_3) #AIC: 1839.4
vif(model_3)


# Let us exclude  variable MaritalStatus.xMarried as its p-value is too high (0.108169)
model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkingHours + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xMarketing + EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_4) #AIC: 1840.1
vif(model_4)


# Let us exclude BusinessTravel.xTravel_Rarely as its VIF value is high (4.614043) and p-value is high too (0.005947)
model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + EducationField.xOther + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])

summary(model_5) #AIC: 1846.7
vif(model_5)

# EducationField.xMarketing's p-value is high (0.091269), let us exlcude it

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Department.xSales + EducationField.xOther + 
                 JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_6) #AIC: 1847.6
vif(model_6)

# EducationField.xOther has high p-value (0.044371), let us exclude it

model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Department.xSales + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_7) #AIC: 1850
vif(model_7)

# JobRole.xResearch.Director's p-value is high (0.01474), let us remove it

model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Department.xSales + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])

summary(model_8) #AIC : 1853.7
vif(model_8)

# WorkLifeBalance.x4's VIF ( 2.168911) is high and high p-value (0.000619), let us exclude it

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 AvgWorkingHours + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                 Department.xSales + JobRole.xHuman.Resources + JobRole.xManager + 
                 JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train[, -1])

summary(model_9) #AIC : 1863.4
vif(model_9)

# WorkLifeBalance.x2's p-value (0.013090) is high, let us exclude

model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + Department.xResearch...Development + 
                  Department.xSales + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  JobInvolvement.x3, family = "binomial", data = train[, -1])

summary(model_10) # AIC: 1867.5
vif(model_10)

# Department.xResearch...Development's vif is high, let us exclude it

model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  Department.xSales + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x3, 
                family = "binomial", data = train[, -1])

summary(model_11) # AIC: 1886.8
vif(model_11)

# Exclude variable Department.xSales next
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently +  
                  JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x3, 
                family = "binomial", data = train[, -1])

summary(model_12) #AIC: 1889.6
vif(model_12)

# Exclude variable JobRole.xHuman.Resources next
model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently +  
                  JobRole.xManager + JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x3, 
                family = "binomial", data = train[, -1])

summary(model_13) #AIC: 1896.1
vif(model_13)

# Exclude variable JobRole.xManager next
model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + 
                  JobRole.xManufacturing.Director + MaritalStatus.xSingle + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + JobInvolvement.x3, 
                family = "binomial", data = train[, -1])

summary(model_14) #AIC: 1903.2
vif(model_14)

# Exclude variable JobInvolvement.x3 next
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3, 
                family = "binomial", data = train[, -1])

summary(model_15) #AIC: 1909.5
vif(model_15)

# Exclude variable JobSatisfaction.x3 next
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                data = train[, -1])
summary(model_16)
vif(model_16)

# Exclude variable JobSatisfaction.x2 next
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3, family = "binomial", 
                data = train[, -1])
summary(model_17) #AIC: 1919.5
vif(model_17)

# Exclude variable WorkLifeBalance.x3 next
model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgWorkingHours + BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4, family = "binomial", 
                data = train[, -1])
summary(model_18) # AIC: 1926.6
vif(model_18)

#
# We are left with 12 predictor variables now. All have low VIF < 2 and low p-value
# This seems to be an fairly optimal model. Let us evaluate the model now.
#
#Age, NumCompaniesWorked 
#TotalWorkingYears, TrainingTimesLastYear 
#YearsSinceLastPromotion, YearsWithCurrManager 
#AvgWorkingHours, BusinessTravel.xTravel_Frequently 
#JobRole.xManufacturing.Director, MaritalStatus.xSingle 
#EnvironmentSatisfaction.x2, EnvironmentSatisfaction.x3 
#EnvironmentSatisfaction.x4, JobSatisfaction.x4 


#
### Final Model Evaluation
#

final_model <- model_18
summary(final_model)

### Test Data ####

#
#predicted probabilities of Attrition for test data
#
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-3])

#
# Let's see the summary 
#

summary(test_pred)

test$prob <- test_pred
View(test)

#
# Let's use a probability cutoff of 50% & see if it's OK
#
test_pred_attrition <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#test_actual_attrition  No    Yes
#                   No  918   131
#                   Yes 42    63
#
#
#Accuracy of the Model is 85%
#Sensitivity : 32%         
#Specificity : 96%  

#This cutoff value is not optimal, as we need higher sensitivity (TP about Attrition)

#########################################################################################
# Let's determine the optimal cutoff probability value 
# 

#
# This function returns the 3 parameters : Accuracy, Sensitivity and Specifivity
# for the input probability cutoff value
#
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from 0.01 to 0.86 for plotting 
# and initiallizing a matrix of 1000 X 3.

# Summary of test probability
summary(test_pred)

s = seq(0.01,0.86,length=1000)

OUT = matrix(0,1000,3)

for(i in 1:1000)
{
  OUT[i,] = perform_fn(s[i])
} 


#
# Plot the 3 parameters : Accuracy, Sensitivity and Specifivity
# for the probability range (0.01,0.86)
#
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.45,.70,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff <- mean(cutoff)

#
# Therefore, the optimal cutoff value for our model 
# is 0.186977 - which gives us the best accuracy and sensitivity (TP %)
#
cutoff # 0.186977

# Let's use the cutoff value of 0.186977 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc  # 0.7487002
sens # 0.742268
spec # 0.75

View(test)

#
# Now let us determine the goodness of our model and
# chosen cutoff value using the Gain, Lift and KS-Statistic methods
#

##############################################################################
#KS -statistic - Test Data By Sharma Method ######
# Ref: https://datascience.stackexchange.com/questions/19493/what-is-a-good-method-to-generate-the-ks-statistic-in-r

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

plot(performance_measures_test,main=paste0(' Sharma Method: KS=',round(max(ks_table_test)*100,1),'%'))
lines(x = c(0,1),y=c(0,1), col = "red")

max(ks_table_test)
#[1] 0.492268 # Its > 40% 

#####################################################################
#calculating area under curve
auc <- performance(pred_object_test,measure="auc")
auc <- auc@y.values[[1]]
print(auc)
#[1] 0.746134


#####################################################################

####################################################################

# Lift & Gain Chart 

#
# This function calculates the Gain, Lift and KS values for all input
# deciles
#
lift <- function(labels, predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))

  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)

    gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           CumNresp = cumsum(total - totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           NoGain = CumNresp/sum(total-totalresp)*100,
           KS = Gain-NoGain,
           Cumlift=Gain/(bucket*(100/groups)))

    return(gaintable)
}

#
# This data frame contains our model's gain, lift and KS values
# but it doesn't have the perfect values.
#
Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

#
# Now, let us calculate the Perfect Gain values
#
Attrition_decile$perfect <- 0
Attrition_decile$Cumperfect <- 0
tmp<- sum(Attrition_decile$totalresp)
for(i in 1:10) {

    ifelse( tmp - Attrition_decile[i, "total"] >= 0,
            Attrition_decile[i, "perfect"] <- Attrition_decile[i, "total"],
            Attrition_decile[i, "perfect"] <- tmp
    )
    tmp <- tmp- Attrition_decile[i, "perfect"]
    Attrition_decile[i, "Cumperfect"] <- sum(Attrition_decile$perfect)/sum(Attrition_decile$totalresp)*100
}

View(Attrition_decile)

#
# KS -Statistic by our method
#
print(max(Attrition_decile$KS) ) # 51.84171
print(Attrition_decile[Attrition_decile$KS == max(Attrition_decile$KS), "bucket"]) # Bucket number = 3

#
# Bucket where we got the KS-Statistic
#
Attrition_decile$Gain - (Attrition_decile$bucket*10)

#
# Thus we saw that the KS-Statistic obtained using our method (51.8)
# and using the Sharma method (49.2) are almost near
#

#
# Plot the Lift table
#
library(plotrix)
twoord.plot( main = paste("Lift Chart"), 
             rx= 1:10,
             lx=1:10,
             lylim = c(-75, 75),
             rylim = c(-5, 5),
             ly=Attrition_decile$Gain - (Attrition_decile$bucket*10), 
             ry = seq(0,0,length=10), 
             ylab="Our Model: Lift %",
             rylab="Random Model Gain %",
             xlab = "Deciles")

#
# Plot the Gain table
#
library(plotrix)
twoord.plot( main = paste("Gain Chart"), 
             rx= 1:10,
             lx=1:10,
             ly=Attrition_decile$Gain, 
             ry = seq(1,100,length=10), 
             ylab="Our Model: Gain %",
             rylab="Random Model Gain %",
             xlab = "Deciles")


#
# Plot our model's Gain vs. Random Gain vs. Perfect Gain
#

plot(1:10, Attrition_decile$Gain,xlab="Decile",ylab="Gain",cex.lab=1.5,cex.axis=10,ylim=c(0,100),type="l",lwd=2,axes=FALSE,col=2)
axis(1,1:10,1:10)
axis(2,seq(0,110,length=12),seq(0,110,length=12),cex.lab=11)
lines(1:10,Attrition_decile$bucket*10,col="darkgreen",lwd=2)
lines(1:10,Attrition_decile$Cumperfect,col=4,lwd=2)
box()
legend(5.85,47.70,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Our Model","Random","Perfect"))
