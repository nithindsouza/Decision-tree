###########################Problem 1##############################
#load company data
company_data <- read.csv("C:\\Users\\hp\\Desktop\\Decision tree assi\\Company_Data.csv" , stringsAsFactors = TRUE)
summary(company_data$Sales)

#categorizing the sales column (target) using cut function
company_data$Sales <- cut(company_data$Sales , breaks = c( -Inf ,8.135, Inf)
                          , labels = c("low" , "high"))

company_data$Sales <- as.factor(company_data$Sales )
str(company_data)

#splitting the data
library(caTools)
set.seed(2)
split <- sample.split(company_data$Sales, SplitRatio = 0.8)
company_train <- subset(company_data, split == TRUE)
company_test <- subset(company_data, split == FALSE)

# check the proportion of class variable
prop.table(table(company_data$Sales))
prop.table(table(company_train$Sales))
prop.table(table(company_test$Sales))

#Training a model on the data
install.packages("C50")
library(C50)

#model 
model1 <- C5.0(company_train[, -1], company_train$Sales)

#plotting the model
windows()
plot(model1)

# Display detailed information about the tree
summary(model1)

#Evaluating model performance
# Test data accuracy
test_res <- predict(model1, company_test)
test_acc <- mean(company_test$Sales == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(company_test$Sales, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model1, company_train)
train_acc <- mean(company_train$Sales == train_res)
train_acc

table(company_train$Sales, train_res)

#train accuracy more than test accuracy hence overfit model

#building the model using train data using pruning technique to overcome overfit
library(rpart)
model2 <- rpart(Sales ~ ., data = company_train,method = 'class',control = rpart.control(cp = 0, maxdepth = 3))

# Plot Decision Tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model2, box.palette = "auto", digits = 1)

# Test data accuracy
test_res <- predict(model2, company_test , type = 'class')
test_acc <- mean(company_test$Sales == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(company_test$Sales, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model2, company_train , type = 'class')
train_acc <- mean(company_train$Sales == train_res)
train_acc

table(company_train$Sales, train_res)

#both train and test accuracy are alomost same and hence right fit model

#random forest method
install.packages("randomForest")
library(randomForest)

#building random forest model
model3 <- randomForest(Sales ~ ., data = company_train,maxnodes=4, mtree=6)
help(randomForest)
#plotting the model
windows()
plot(model3)

# Test data accuracy
test_res <- predict(model3, company_test , type = 'class')
test_acc<- mean(company_test$Sales == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(company_test$Sales, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model3, company_train , type = 'class')
train_acc <- mean(company_train$Sales == train_res)
train_acc
table(company_train$Sales, train_res)

######################################Problem 2#################################
#load company data
diabetes_data <- read.csv("C:\\Users\\hp\\Desktop\\Decision tree assi\\Diabetes.csv" , stringsAsFactors = TRUE)
summary(diabetes_data$Class.variable)

str(diabetes_data)

#splitting the data
library(caTools)
set.seed(2)
split <- sample.split(diabetes_data$Class.variable, SplitRatio = 0.8)
diabetes_train <- subset(diabetes_data, split == TRUE)
diabetes_test <- subset(diabetes_data, split == FALSE)

# check the proportion of class variable
prop.table(table(diabetes_data$Class.variable))
prop.table(table(diabetes_train$Class.variable))
prop.table(table(diabetes_test$Class.variable))

#Training a model on the data
install.packages("C50")
library(C50)

#model 
model1 <- C5.0(diabetes_train[, -9], diabetes_train$Class.variable)

#plotting the model
windows()
plot(model1)

# Display detailed information about the tree
summary(model1)

#Evaluating model performance
# Test data accuracy
test_res <- predict(model1, diabetes_test)
test_acc <- mean(diabetes_test$Class.variable == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(diabetes_test$Class.variable, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model1, diabetes_train)
train_acc <- mean(diabetes_train$Class.variable == train_res)
train_acc

table(diabetes_train$Class.variable, train_res)


#building the model using train data using pruning technique
library(rpart)
model2 <- rpart(Class.variable ~ ., data = diabetes_train,method = 'class',control = rpart.control(cp = 0, maxdepth = 4))

# Plot Decision Tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model2, box.palette = "auto", digits = -1)

# Test data accuracy
test_res <- predict(model2, diabetes_test , type = 'class')
test_acc <- mean(diabetes_test$Class.variable == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(diabetes_test$Class.variable, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model2, diabetes_train , type = 'class')
train_acc <- mean(diabetes_train$Class.variable == train_res)
train_acc

table(company_train$Sales, train_res)

#both train and test accuracy are alomost same and hence right fit model

#random forest method
install.packages("randomForest")
library(randomForest)

#building random forest model
model3 <- randomForest(Class.variable ~ ., data = diabetes_train,maxnodes=4, mtree=6)
help(randomForest)
#plotting the model
windows()
plot(model3)

# Test data accuracy
test_res <- predict(model3, diabetes_test , type = 'class')
test_acc<- mean(diabetes_test$Class.variable == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(diabetes_test$Class.variable, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model3, diabetes_train , type = 'class')
train_acc <- mean(diabetes_train$Class.variable == train_res)
train_acc
table(diabetes_train$Class.variable, train_res)
##########################################Problem 3#################################################
#load company data
fraud_check_data <- read.csv("C:\\Users\\hp\\Desktop\\Decision tree assi\\Fraud_check.csv" , stringsAsFactors = TRUE)

#categorizing the Taxable.Income  column (target) using cut function
fraud_check_data$Taxable.Income <- cut(fraud_check_data$Taxable.Income , breaks = c( -Inf ,30000, Inf)
                          , labels = c("Risky" , "Good"))

summary(fraud_check_data$Taxable.Income)

fraud_check_data$Taxable.Income <- as.factor(fraud_check_data$Taxable.Income )
str(fraud_check_data)

# check the proportion of class variable
prop.table(table(fraud_check_data$Taxable.Income))

# #since imbalanced data we upsample
# install.packages("caret")
# library(caret)
# fraud_check <- upSample(fraud_check_data,fraud_check_data$Taxable.Income)

#splitting the data
library(caTools)
set.seed(2)
fraud_check <- fraud_check_data
split <- sample.split(fraud_check$Taxable.Income, SplitRatio = 0.7)
fraud_check_train <- subset(fraud_check, split == TRUE)
fraud_check_test <- subset(fraud_check, split == FALSE)

# check the proportion of class variable
prop.table(table(fraud_check$Taxable.Income))
prop.table(table(fraud_check_train$Taxable.Income))
prop.table(table(fraud_check_test$Taxable.Income))

#Training a model on the data
install.packages("C50")
library(C50)

#model 
model1 <- C5.0(fraud_check_train[, -c(3)], fraud_check_train$Taxable.Income)

#plotting the model
windows()
plot(model1)

# Display detailed information about the tree
summary(model1)

#Evaluating model performance
# Test data accuracy
test_res <- predict(model1, fraud_check_test)
test_acc <- mean(fraud_check_test$Taxable.Income == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(fraud_check_test$Taxable.Income, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model1, fraud_check_train)
train_acc <- mean(fraud_check_train$Taxable.Income == train_res)
train_acc

table(fraud_check_train$Taxable.Income, train_res)


#building the model using train data using pruning technique
library(rpart)
model2 <- rpart(Taxable.Income ~ ., data = fraud_check_train,method = 'class',control = rpart.control(cp = 0, maxdepth = 4))

# Plot Decision Tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model2, box.palette = "auto", digits = -1)

# Test data accuracy
test_res <- predict(model2, fraud_check_test , type = 'class')
test_acc <- mean(fraud_check_test$Taxable.Income == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(fraud_check_test$Taxable.Income, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model2, fraud_check_train , type = 'class')
train_acc <- mean(fraud_check_train$Taxable.Income == train_res)
train_acc

table(fraud_check_train$Taxable.Income, train_res)

#both train and test accuracy are alomost same and hence right fit model

#random forest method
install.packages("randomForest")
library(randomForest)

#building random forest model
model3 <- randomForest(Taxable.Income ~ ., data = fraud_check_train,maxnodes=4, mtree=6)
help(randomForest)
#plotting the model
windows()
plot(model3)

# Test data accuracy
test_res <- predict(model3, fraud_check_test , type = 'class')
test_acc<- mean(fraud_check_test$Taxable.Income == test_res)
test_acc

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(fraud_check_test$Taxable.Income, test_res, dnn = c('actual default', 'predicted default'))

# On Training Dataset
train_res <- predict(model3, fraud_check_train , type = 'class')
train_acc <- mean(fraud_check_train$Taxable.Income == train_res)
train_acc
table(fraud_check_train$Taxable.Income, train_res)

#############################################Problem 4###############################################
#load company data
HR_data <- read.csv("C:\\Users\\hp\\Desktop\\Decision tree assi\\HR_DT.csv" , stringsAsFactors = TRUE)
summary(HR_data$monthly.income.of.employee)

library(caTools)
set.seed(2)
split <- sample.split(HR_data$monthly.income.of.employee, SplitRatio = 0.8)
HR_train <- subset(HR_data, split == TRUE)
HR_test <- subset(HR_data, split == FALSE)

library(rpart)
model <- rpart(monthly.income.of.employee ~ ., data = HR_train,method = 'class',control = rpart.control(cp = 0, maxdepth = 5))
help("rpart")

# Plot Decision Tree
library(rpart.plot)
rpart.plot(model, box.palette = "auto", digits = -3)

# Measure the RMSE on Test data hence test accuracy
test_pred <- predict(model, newdata = HR_test, type = "vector")
acc_test <- sqrt(mean(HR_test$monthly.income.of.employee - test_pred)^2)
acc_test

# Measure the RMSE on Train data hence train accuracy
train_pred <- predict(model, newdata = HR_train, type = "vector")
acc_train <- sqrt(mean(HR_train$monthly.income.of.employee - train_pred)^2)
acc_train
#since both the RMSE of train and test are similar the model is right fit

#Random forest model
library(randomForest)
model <- randomForest(monthly.income.of.employee ~ ., data = HR_train , maxnodes=4, mtree=3)

# Measure the RMSE on Test data hence test accuracy
test_pred <- predict(model, newdata = HR_test)
acc_test <- sqrt(mean(HR_test$monthly.income.of.employee - test_pred)^2)
acc_test

# Measure the RMSE on Train data hence train accuracy
train_pred <- predict(model, newdata = HR_train)
acc_train <- sqrt(mean(HR_train$monthly.income.of.employee - train_pred)^2)
acc_train

#now checking that the candidate claim is genuine or fake
#candidate claims are stored in a dataframe
cols <- c("Position.of.the.employee", "no.of.Years.of.Experience.of.employee","monthly.income.of.employee")
Candidate_claim <- data.frame(a <-  "Region Manager" , b <-  5 , c <-  70000)
colnames(Candidate_claim) <- cols
#binding with test data for prediction
HR_test <- rbind(HR_test, Candidate_claim)

#predicting using model
HR_test$monthly.income.of.employee.pred <- predict(model, newdata = HR_test)

#our predicted salary is (61131.74)
HR_test$monthly.income.of.employee.pred[31]

#since the  predicted salary is almost similar to claimed salary it is said that candidate is genuine 

###################################################END############################################
