### This R script serves as the backend code for group assignment of Financial Risk Analytics ###

### Loading the required libraries
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(pROC))
suppressMessages(library(e1071))
suppressMessages(library(caret))
suppressMessages(library(janitor))
suppressMessages(library(DataExplorer))
suppressMessages(library(corrplot))



##### Importing & Understanding Dataset #####



## Loading the training data into a R data frame
train_data <- read_excel("raw-data-1.xlsx", na = c(" ", "NA")) %>% clean_names()

## Loading the validation data into a R data frame
val_data <- read_excel("validation_data-1.xlsx", na = c(" ", "NA")) %>% clean_names()

## viewing the dimension of the data frames 
dim(train_data)
dim(val_data)

## Viewing the structure of the data frame
str(train_data)
str(val_data)

## Encoding our depenedent numeric variable to a factor
train_data$networth_next_year <- as.factor(ifelse(train_data$networth_next_year 
                                                  < 0, 1, 0))
# Renaming to "default_1"
names(train_data)[names(train_data) == "networth_next_year"] <- "default_1"

# Variable "deposits_accepted_by_commercial_banks"
summary(train_data$deposits_accepted_by_commercial_banks)

## Dropping variables
train_data <- train_data %>% mutate(num = NULL, deposits_accepted_by_commercial_banks
                                    = NULL)
val_data <- val_data %>% mutate(num = NULL, deposits_accepted_by_commercial_banks 
                                = NULL)

## Viewing the summary of the data frame
summary(train_data)
summary(val_data)



##### Exploratory Data Analysis #####



## The dependent varaible - default_1
table(train_data$default_1)

## Variable "total_assets"
ggplot(train_data, aes(total_assets)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(total_assets))) + geom_histogram(fill = "blue")

# Hypothesis - Companies having higher assets are less likely to default
ggplot(train_data, aes(default_1, log(total_assets))) + geom_boxplot()

## Variable "net_worth"
ggplot(train_data, aes(net_worth)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(net_worth + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which have a very high net worth this year, might not turn into negative next year, i.e. less likely to default.
ggplot(train_data, aes(default_1, log(net_worth + 1))) + geom_boxplot()

## Variable "total_income"
ggplot(train_data, aes(total_income)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(total_income + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, having low total income, are more likely to default.
ggplot(train_data, aes(default_1, log(total_income + 1))) + geom_boxplot()

## Variable "change_in_stock"
ggplot(train_data, aes(change_in_stock)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(change_in_stock + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which defaulted, have a less change in stock, compared to companies which didn't
ggplot(train_data, aes(default_1, log(change_in_stock + 1))) + geom_boxplot()

## Variable "total_expenses"
ggplot(train_data, aes(total_expenses)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(total_expenses + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which defaulted, have higher expenses, compared to companies which didn't
ggplot(train_data, aes(default_1, log(total_expenses + 1))) + geom_boxplot()

## Variable "profit_after_tax"
ggplot(train_data, aes(profit_after_tax)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(profit_after_tax + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which defaulted, have lower profit after tax, compared to companies which didn't
ggplot(train_data, aes(default_1, log(profit_after_tax + 1))) + geom_boxplot()

## Variable "pbt"
ggplot(train_data, aes(pbt)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(pbt + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which defaulted, have lower profit before tax, compared to companies which didn't
ggplot(train_data, aes(default_1, log(pbt + 1))) + geom_boxplot()

## Variable "cash_profit"
ggplot(train_data, aes(cash_profit)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(cash_profit + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which defaulted, have lower cash profit, compared to companies which didn't
ggplot(train_data, aes(default_1, log(cash_profit + 1))) + geom_boxplot()

## Variable "sales"
ggplot(train_data, aes(sales)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(sales + 1))) + geom_histogram(fill = "blue")

# Hypothesis - Companies, which defaulted, have a lower sales, compared to those which didn't.
ggplot(train_data, aes(default_1, log(sales + 1))) + geom_boxplot()

## Variable "reserves_and_funds"
ggplot(train_data, aes(reserves_and_funds)) + geom_histogram(fill = "blue")
# Taking log transformation
ggplot(train_data, aes(log(reserves_and_funds + 1))) + geom_histogram(fill = "blue")



##### Feature Engineering #####



## Combining training & validation dataset
comb_data <- rbind(train_data, val_data)

## Creating ratio variables
comb_data$total_income_to_total_assets <- comb_data$total_income / comb_data$total_assets
comb_data$change_in_stock_to_total_income <- comb_data$change_in_stock / comb_data$total_income
comb_data$total_expenses_to_total_income <- comb_data$total_expenses / comb_data$total_income
comb_data$pat_to_total_assets <- comb_data$profit_after_tax / comb_data$total_assets
comb_data$pbdita_to_total_assets <- comb_data$pbdita / comb_data$total_assets
comb_data$pbt_to_total_assets <- comb_data$pbt / comb_data$total_assets
comb_data$cash_profit_to_total_assets <- comb_data$cash_profit / comb_data$total_assets
comb_data$sales_to_total_assets <- comb_data$sales / comb_data$total_assets
comb_data$income_from_financial_services_to_total_income <- 
    comb_data$income_from_financial_services / comb_data$total_income
comb_data$other_income_to_total_income <- comb_data$other_income / comb_data$total_income
comb_data$total_capital_to_total_assets <- comb_data$total_capital / comb_data$total_assets
comb_data$reserves_and_funds_to_total_assets <- comb_data$reserves_and_funds / comb_data$total_assets
comb_data$borrowings_to_total_assets <- comb_data$borrowings / comb_data$total_assets
comb_data$current_liabilities_provisions_to_total_assets <- 
    comb_data$current_liabilities_provisions / comb_data$total_assets
comb_data$deferred_tax_liability_to_total_assets <- 
    comb_data$deferred_tax_liability / comb_data$total_assets
comb_data$shareholders_funds_to_total_assets <- comb_data$shareholders_funds / comb_data$total_assets
comb_data$cumulative_retained_profits_to_total_income <- 
    comb_data$cumulative_retained_profits / comb_data$total_income
comb_data$capital_employed_to_total_assets <- comb_data$capital_employed / comb_data$total_assets
comb_data$net_fixed_assets_to_total_assets <- comb_data$net_fixed_assets / comb_data$total_assets
comb_data$investments_to_total_income <- comb_data$investments / comb_data$total_income
comb_data$current_assets_to_total_assets <- comb_data$current_assets / comb_data$total_assets
comb_data$net_working_capital_to_total_capital <- 
    comb_data$net_working_capital / comb_data$total_capital



##### Data Preparation & Cleaning #####



## Dropping variables no more required
vars_to_drop <- c("total_assets", "total_income", "change_in_stock", "total_expenses",
                  "profit_after_tax", "pbdita", "pbt", "cash_profit", "sales", 
                  "income_from_financial_services", "other_income", "total_capital",
                  "reserves_and_funds", "borrowings", "current_liabilities_provisions", 
                  "deferred_tax_liability", "shareholders_funds", "cumulative_retained_profits",
                  "capital_employed", "net_fixed_assets", "investments", "current_assets",
                  "net_working_capital")

comb_data_reduced <- comb_data %>% select(-vars_to_drop)
  

## Missing Values

# Total missing values
plot_intro(comb_data_reduced)

# Missing values per column
plot_missing(comb_data_reduced)

# Dropping variables with more than 40% missing values
comb_data_reduced <- drop_columns(comb_data_reduced, c("investments_to_total_income", 
                                                       "pe_on_bse"))

# Imputing with median for rest of the variables
imputed_data <- data.frame(lapply(comb_data_reduced,function(x) {
  if(is.numeric(x)) ifelse(is.na(x),median(x,na.rm=T),x) else x}))

# Rechecking Missing Values
sum(is.na(imputed_data))



##### Dimensionality Reduction #####



# Identifying High Correlations
cor_matrix <- imputed_data %>% select_if(is.numeric) %>% cor(use = "pairwise.complete.obs")
high_correlations <- findCorrelation(cor_matrix, cutoff = 0.8, verbose = T, names = T)

# Dropping Highly Correlated Variables
imputed_data <- drop_columns(imputed_data, c("pbdita_to_total_assets", 
                                             "change_in_stock_to_total_income", 
                                             "pat_to_total_assets", 
                                             "other_income_to_total_income", 
                                             "pbt_to_total_assets",
                                             "cumulative_retained_profits_to_total_income"))



##### Model Building #####



## Splitting data into train & test
train_data <- imputed_data[1:3541, ]
val_data <- imputed_data[3542:4256, ]

# Building a logistic regression model
logit.fit <- glm(default_1 ~ ., data = train_data, family = binomial(link = "logit"))         
logit.fit

# Summary of logistic model
summary(logit.fit)

# Using stepwise variable selection to minimize AIC
step(logit.fit <- glm(default_1 ~ ., data = train_data, family = binomial(link = "logit")))

# Rebuilding the logistic model
logit.fit1 <- glm(formula = default_1 ~ net_worth + pbt_as_percent_of_total_income + 
                    cash_profit_as_percent_of_total_income + pat_as_percent_of_net_worth + 
                    contingent_liabilities + debt_to_equity_ratio_times + cash_to_average_cost_of_sales_per_day + 
                    finished_goods_turnover + total_liabilities + 
                    sales_to_total_assets + total_capital_to_total_assets + borrowings_to_total_assets + 
                    current_liabilities_provisions_to_total_assets + capital_employed_to_total_assets + 
                    net_fixed_assets_to_total_assets + current_assets_to_total_assets, 
                  family = binomial(link = "logit"), data = train_data)

summary(logit.fit1)

# Predicting on the train set
logit.pred <- predict(logit.fit1, newdata = train_data, type = "response")

# AUC-ROC for the model
auc(train_data$default_1, logit.pred)

# Plotting ROC Curve
plot(roc(train_data$default_1, logit.pred))

# Setting the threshold value
logit.pred1 <- ifelse(logit.pred > 0.50, 1, 0)

# Confusion matrix
confusionMatrix(train_data$default_1, as.factor(logit.pred1), positive = "1")

# Predicting on the validation set
logit.pred2 <- predict(logit.fit1, newdata = val_data, type = "response")

# AUC-ROC for the model
auc(val_data$default_1, logit.pred2)

# Plotting ROC Curve
plot(roc(val_data$default_1, logit.pred2))

# Setting the threshold value
logit.pred3 <- ifelse(logit.pred2 > 0.50, 1, 0)

# Confusion matrix
confusionMatrix(val_data$default_1, as.factor(logit.pred3), positive = "1")



##### Model Improvement ##### 



# Treating missing values using knn algorithm
preprocessed_data <- preProcess(comb_data_reduced[ ,-43], method = "knnImpute")

# Predicting missing values using above model
library(RANN)
cleaned_data <- predict(preprocessed_data, newdata = comb_data_reduced[ ,-43])

# Setting dependent variable names
cleaned_data$default_1 <- as.factor(ifelse(cleaned_data$default_1 == 1, "Yes", "No"))

# Separating out training and validation set
cv_train <- cleaned_data[1:3541, ]
cv_val <- cleaned_data[3542:4256, ]

# Setting up train control
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, 
                           classProbs = T, savePredictions = "final", 
                           sampling = "smote")

# Performance metric
metric = "accuracy"

# Building 10 fold cross validation model
set.seed(1985)
cv_fit <- train(x = cv_train[ ,-1], y = cv_train$default_1, method = "glm", 
                trControl = fitControl, tuneLength = 10)

cv_fit

# Predicting on validation set
cv_pred <- predict(cv_fit, newdata = cv_val, type = "prob")

# AUC-ROC for the model
auc(cv_val$default_1, cv_pred[[2]])

# Plotting ROC Curve
plot(roc(cv_val$default_1, cv_pred[[2]]))

# Setting the threshold value
cv_pred2 <- as.factor(ifelse(cv_pred[[2]] > 0.80, "Yes", "No"))

# Confusion matrix
confusionMatrix(cv_val$default_1, cv_pred2, positive = "Yes")
