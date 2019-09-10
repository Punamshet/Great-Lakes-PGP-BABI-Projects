### This R script serves as the backend code for group assignment of predictive modeling ###



##### Importing & Understanding the Data #####
install.packages("caret")


### Loading the required libraries
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(pROC))
suppressMessages(library(e1071))

## Loading the data into a R data frame
churn_data <- read_excel("GA_Dataset-1.xlsx", sheet = "Data")

## viewing the dimension of the data frame 
dim(churn_data)

## Viewing the structure of the data frame
str(churn_data)

## Viewing the summary of the data frame
summary(churn_data)

## Converting numeric variables to factor
churn_data$Churn <- as.factor(churn_data$Churn)
churn_data$ContractRenewal <- as.factor(churn_data$ContractRenewal)
churn_data$DataPlan <- as.factor(churn_data$DataPlan)

## Checking unique values in CustServCalls
unique(churn_data$CustServCalls)

## Converting CustServCalls to factor
churn_data$CustServCalls <- factor(churn_data$CustServCalls, ordered = T)

## Summary of the dataset
summary(churn_data)



##### Exploratory Data Analysis #####



## The dependent varaible - Churn
table(churn_data$Churn)

## Variable "AccountWeeks"
ggplot(churn_data, aes(AccountWeeks)) + geom_histogram(fill = "blue")

# Hypothesis - Customer churn is dependent on AccountWeeks
ggplot(churn_data, aes(Churn, AccountWeeks)) + geom_boxplot()

## Variable "ContractRenewal"
table(churn_data$ContractRenewal)

# Hypothesis - Customers, who have not renewed their contract, are more prone to churn
ggplot(churn_data, aes(ContractRenewal, fill = Churn)) + geom_bar(position = "fill")

## Variable "DataPlan"
table(churn_data$DataPlan)

# Hypothesis - Customers, who have a data plan, are less likely to churn
ggplot(churn_data, aes(DataPlan, fill = Churn)) + geom_bar(position = "fill")

## Variable "DataUsage"
ggplot(churn_data, aes(DataUsage)) + geom_histogram(fill = "blue")
# Converting numeric variable to factor
churn_data$DataUsage <- cut(churn_data$DataUsage, breaks = c(-Inf, 0, 2, 4, Inf),
                            labels = c("Zero", "Low", "Medium", "High"))
# Checking the distribution now
table(churn_data$DataUsage)

# Hypothesis - Customers, having a data plan, but using less data, are more likely to churn
ggplot(churn_data, aes(DataUsage, fill = Churn)) + geom_bar(position = "fill") +
  facet_wrap(~ churn_data$DataPlan)

## Variable "CustServCalls"
table(churn_data$CustServCalls)
# Clubbing few factor levels
churn_data$CustServCalls <- ifelse(as.character(churn_data$CustServCalls) >= "5", 
                                   "5+", as.character(churn_data$CustServCalls))
churn_data$CustServCalls <- as.factor(churn_data$CustServCalls)

# Hypothesis - Customers, who make more service calls, are annoyed with the
# service provided and hence prone to churn
ggplot(churn_data, aes(CustServCalls, fill = Churn)) + geom_bar(position = "fill")

## Variable "DayMins"
ggplot(churn_data, aes(DayMins)) + geom_histogram(fill = "blue")

# Hypothesis: Customers, who churned, have a higher mean day time calling minutes.	
ggplot(churn_data, aes(Churn, DayMins)) + geom_boxplot()

## Variable "DayCalls"
ggplot(churn_data, aes(DayCalls)) + geom_histogram(fill = "blue")

# Customers with "Zero" number of calls
churn_data %>% filter(DayCalls == 0)

ggplot(churn_data, aes(Churn, DayCalls)) + geom_boxplot()

## Variable "MonthlyCharge"
ggplot(churn_data, aes(MonthlyCharge)) + geom_histogram(fill = "blue")

# Hypothesis: Customers, who don't have a data plan, but have data usage, their 
# monthly charges will be higher and hence they tend to churn more.
ggplot(churn_data, aes(DataUsage, MonthlyCharge, fill = Churn)) + geom_boxplot() +
  facet_wrap(~ churn_data$DataPlan)

## Variable "OverageFee"
ggplot(churn_data, aes(OverageFee)) + geom_histogram(fill = "blue")

# Hypothesis: Customers, who had paid more overage fee, are most likely to churn.
ggplot(churn_data, aes(Churn, OverageFee, fill = Churn)) + geom_boxplot() 

## Variable "RoamMins"
ggplot(churn_data, aes(RoamMins)) + geom_histogram(fill = "blue")

# Hypothesis: Customers, who have higher number of roaming minutes, might have 
# paid higher overage fees and hence they would like to migrate to other service 
# providers, who have cheaper tariffs.
ggplot(churn_data, aes(RoamMins, OverageFee, col = Churn)) + geom_point() 



##### Model Building #####
install.packages("caret", dependencies = T)


## Splitting data into train & test
set.seed(1985)
index <- createDataPartition(churn_data$Churn, p = 0.70, list = F) # Sampling 70% data from the original data
train <- churn_data[index, ]
test <- churn_data[-index, ]

# Checking proportion of churn in train and test dataset
prop.table(table(churn_data$Churn))
prop.table(table(train$Churn))
prop.table(table(test$Churn))

# Building a logistic regression model
logit.fit <- glm(Churn ~ ., data = train, family = "binomial")
logit.fit

# Summary of logistic model
summary(logit.fit)

# Dropping insignificant variables and rebuilding our model
logit.fit1 <- glm(Churn ~ ContractRenewal + DataPlan + DataUsage + CustServCalls + 
                  MonthlyCharge + RoamMins, family = "binomial", data = train)
summary(logit.fit1)

# Predicting on the test set
logit.pred <- predict(logit.fit1, newdata = test, type = "response")

# AUC-ROC for the model
auc(test$Churn, logit.pred)

# Plotting ROC Curve
plot(roc(test$Churn, logit.pred))

# Adjusting the threshold value
logit.pred1 <- ifelse(logit.pred > 0.20, 1, 0)

# Confusion matrix
confusionMatrix(test$Churn, as.factor(logit.pred1), positive = "1")
