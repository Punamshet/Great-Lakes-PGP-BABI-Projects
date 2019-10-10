############################################################################
## This script is the solution code for assigment problem for supply chain 
## & logistics analytics.
############################################################################

# Loading required libraries
library(lubridate) # working with dates
library(dplyr) # data manipulation
library(ggplot2) # data visualization
library(Hmisc) # describe function
library(caret) # cross validated models
library(pROC) # ROC plot


## Loading the data into a R data frame
inventory <- read.csv("Inventory.csv")

## viewing the dimension of the data frame 
dim(inventory)

## Viewing the structure of the data frame
str(inventory)

## Concise statistical description using describe() from Hmisc package
describe(inventory)

## Formating date variable
inventory$Order.Date <- parse_date_time(inventory$Order.Date, orders = "mdy")

## Converting Product Name to character
inventory$Product.Name <- as.character(inventory$Product.Name)

## Summary of the dataset
summary(inventory)



##### Exploratory Data Analysis #####



## The dependent varaible - Ship Mode
table(inventory$Ship.Mode)
# Checking the class proportions
prop.table(table(inventory$Ship.Mode))

## Variable "Order Quantity"
ggplot(inventory, aes(Order.Quantity)) + geom_histogram(fill = "blue")

# Hypothesis - Ship Mode is not dependent on Order Quantity
ggplot(inventory, aes(Ship.Mode, Order.Quantity)) + geom_boxplot()

## Variable "Product Container"
table(inventory$Product.Container)

# Hypothesis - Product Container type determines the type of Ship Mode
table(inventory$Product.Container, inventory$Ship.Mode)
# Graphical representation of above table
ggplot(inventory, aes(Product.Container, fill = Ship.Mode)) + 
  geom_bar(position = "fill")

# Hypothesis - Order Quantity and Product Container both together affects Ship Mode
ggplot(inventory, aes(Product.Container, Order.Quantity, fill = Ship.Mode)) + 
  geom_boxplot()

## Variable "Product Sub-Category"
table(inventory$Product.Sub.Category)

# Hypothesis - Product Sub-Category determines the type of Ship Mode
ggplot(inventory, aes(Product.Sub.Category, fill = Ship.Mode)) + 
  geom_bar(position = "fill")

## Variable "Sales"
ggplot(inventory, aes(Sales)) + geom_histogram(fill = "blue")
# Taking log transformation of Sales
ggplot(inventory, aes(log(Sales))) + geom_histogram(fill = "blue")

# Hypothesis - Sales values are impacting Ship Mode decisions
ggplot(inventory, aes(Ship.Mode, log(Sales))) + geom_boxplot()



##### Feature Engineering #####
# Let's try to create some additional features from the Order.Date feature

# Adding day of week feature
inventory$Order.WeekDay <- wday(inventory$Order.Date, label = T)

# Adding day of month feature
inventory$Order.MonthDay <- as.factor(mday(inventory$Order.Date))

# Hypothesis - Day of Week has an impact on Ship.Mode
ggplot(inventory, aes(Order.WeekDay, fill = Ship.Mode)) + 
  geom_bar(position = "fill")

chisq.test(inventory$Order.WeekDay, inventory$Ship.Mode)

# Hypothesis - Day of Month has an impact on Ship.Mode
ggplot(inventory, aes(Order.MonthDay, fill = Ship.Mode)) + 
  geom_bar(position = "fill")

chisq.test(inventory$Order.MonthDay, inventory$Ship.Mode)

##### Model Building #####

## Data Preparation ##

# Dropping the variables no more required
inventory <- inventory[ , -c(1, 2, 5, 10)]

# Log transform Sales variable
inventory$Sales <- log(inventory$Sales)

# Renaming the levels of dependent variable
levels(inventory$Ship.Mode) <- c("Delivery_Truck", "Express_Air", "Regular_Air")

# Splitting data into train & test
set.seed(1985)
index <- createDataPartition(inventory$Ship.Mode, p = 0.70, list = F) # Sampling 70% data from the original data
train <- inventory[index, ]
test <- inventory[-index, ]

# Checking proportion of churn in train and test dataset
prop.table(table(inventory$Ship.Mode))
prop.table(table(train$Ship.Mode))
prop.table(table(test$Ship.Mode))

# Seperating Idependent & Dependent variables
train.y <- train$Ship.Mode
train.x <- train[ , -5]
test.y <- test$Ship.Mode
test.x <- test[ , -5]

## Pre-Processing Data ##

# One-Hot Encoding 
dummy.model <- dummyVars( ~ ., data = train.x)
dummy.coded.x <- as.data.frame(predict(dummy.model, newdata = train.x))

# Setting up train control
fitControl <- trainControl(method = "cv", number = 10, classProbs = T, 
                           savePredictions = "final", 
                           summaryFunction = multiClassSummary, 
                           sampling = "smote" )


## Model Building ##

# Building a decision tree model
tree.fit <- train(train.dummy.coded, train.y, method = "rpart", trControl = fitControl, 
                tuneLength = 3, preProcess = c("range"))
tree.fit

# Building a lda model
lda.fit <- train(train.dummy.coded, train.y, method = "lda", trControl = fitControl, 
                  tuneLength = 3, preProcess = c("range"))
lda.fit


# Building a random forest model
rf.fit <- train(train.dummy.coded, train.y, method = "rf", trControl = fitControl, 
                  tuneLength = 3, preProcess = c("range"))
rf.fit


# Building a neural network model
nn.fit <- train(train.x, train.y, method = "nnet", trControl = fitControl, 
                tuneLength = 3, verbose = F, preProcess = c("range"))
nn.fit


# Building a gradient boosting model
gbm.fit <- train(train.dummy.coded, train.y, method = "gbm", trControl = fitControl, 
                tuneLength = 3, verbose = F, preProcess = c("range"))
gbm.fit


# Getting resampling results
resample_result <- resamples(list(DecisionTree = tree.fit,
                                  LDA = lda.fit,
                                  RandomForest = rf.fit,
                                  NeuralNet = nn.fit,
                                  GradientBoosting = gbm.fit))
# Summarizing resampling results
summary(resample_result, metric = c("Accuracy", "AUC", "Kappa"))

# Plotting resampling results
bwplot(resample_result, metric = c("Accuracy", "AUC", "Kappa"))

## We have choosen Gradient Boosting model as our final model. Let's
# look at its contents
gbm.fit

# Predicting on the test set
# For prediction on test set We need to do the same pre-processing as
# done on train set

# Pre-processing test set
# Dummy coding test set
dummy.test.x <- as.data.frame(predict(dummy.model, newdata = test.x))

# Predicting on test set
gbm.pred <- predict(gbm.fit, newdata = dummy.test.x, type = "prob")

# AUC-ROC for the model
multiclass.roc(test.y, gbm.pred)


# Confusion matrix
# getting the response from prediction
pred.response <- predict(gbm.fit, newdata = dummy.test.x, type = "raw")
confusionMatrix(test.y, pred.response)
