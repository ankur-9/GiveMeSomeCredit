library(ggplot2)
library(ROSE)
library(caret)
library(rpart)
library(partykit)
library(rpart.plot)


train <- read.csv("cs-training.csv")
test <- read.csv("cs-test.csv")
head(train)
head(test)
names(train)
dim(train)
summary(train)
train$X <- NULL
test$X <- NULL
# Check the number of missing values
sum(is.na(train))
sum(is.na(test))

#........................................Dealing with MISSING values...............................................#

# For loop to check which column has missing values
train_mis_values <- c() # empty vector to store missing values
for (i in 1:ncol(train)) {
  train_mis_values[i] <- sum(is.na(train[,i]))
  print(paste(colnames(train[i]),train_mis_values[i]))
}



#  Monthly Income has 29731/150000 ~ 20% missing data i.e. 80% data is available so we cannot remove the column from the model
# Impute Monthly Income NAs with median

train[is.na(train$MonthlyIncome),"MonthlyIncome"] <- median(train[,"MonthlyIncome"],na.rm = TRUE)
sum(is.na(train[,"MonthlyIncome"])) # Check


#  NumberOfDependents has 3924/150000 ~ 2.6% missing data which is very small so we can impute with mode value

table(train$NumberOfDependents) # Frequency of each unique value ~ mode
# 87000 cells have 0s and 26000 cells have 1s

train[is.na(train$NumberOfDependents),"NumberOfDependents"] <- 0
sum(is.na(train[,"NumberOfDependents"])) # Check



test_mis_values <- c()
for (i in 2:ncol(test)) {
  test_mis_values[i] <- sum(is.na(test[,i]))
  print(paste(colnames(test[i]),test_mis_values[i]))
}

# Monthly Income has 20103/101503 ~ 20% missing data
# NumberOfDependents has 2626/101503 ~ 2.5% missing data
summary(test) # Outliers in Monthly Income so we will impute median for NAs

test[is.na(test$MonthlyIncome),"MonthlyIncome"]  <- median(test[,"MonthlyIncome"],na.rm = TRUE)
#test1$MonthlyIncome[is.na(test1$MonthlyIncome)] <- median(test1$MonthlyIncome,na.rm = TRUE)
sum(is.na(test$MonthlyIncome))#Check

table(test$NumberOfDependents) # Mode = 0
test$NumberOfDependents[is.na(test$NumberOfDependents)] <- 0
sum(is.na(test$NumberOfDependents)) # Check


#..............................................Dealing with Outliers.............................................#


#RevolvingUtilizationOfUnsecuredLines

ggplot(train,aes(x = 1, y = RevolvingUtilizationOfUnsecuredLines))+
  geom_boxplot(color = "red")+geom_jitter(alpha = 0.1)
quantile(train$RevolvingUtilizationOfUnsecuredLines,c(0,.1,.5,.8,.9,.95,.99,.999,1))
# 0, 0.002 , 0.154, 0.698, 0.98, 1.09, 1571, 50708
# 1571, 50708 are outliers so replace with 1.09
train$RevolvingUtilizationOfUnsecuredLines[train$RevolvingUtilizationOfUnsecuredLines>1.09] <- 1.09

#Age
ggplot(train,aes(x = age))+geom_histogram(color = "black",fill = "orange")
ggplot(train,aes(x = 1, y = age))+geom_boxplot(color = "red")
quantile(train$age,c(0,.1,.5,.8,.9,.95,.99,.999,1))
# Age is normally Distributed

# NumberOfTime30.59DaysPastDueNotWorse
ggplot(train,aes(x = NumberOfTime30.59DaysPastDueNotWorse))+geom_histogram(binwidth = 1, color = "black",fill = "orange")
# Distribution is positively skewed
ggplot(train,aes(x = 1, y = NumberOfTime30.59DaysPastDueNotWorse))+geom_boxplot(color = "red")# Couple of Outliers
quantile(train$NumberOfTime30.59DaysPastDueNotWorse,c(0,.1,.5,.8,.9,.95,.99,.999,1))
train[train$NumberOfTime30.59DaysPastDueNotWorse>4,"NumberOfTime30.59DaysPastDueNotWorse"] <- 4


# DebtRatio
ggplot(train,aes(x = 1, y = DebtRatio))+geom_boxplot(color = "red")# Many Outliers
quantile(train$DebtRatio,c(0,.1,.5,.8,.9,.95,.99,.999,1))
# after 80percentile value jumps from 4 to 1267
train[train$DebtRatio>4,"DebtRatio"]<-4

# Monthly Income
ggplot(train,aes(x = 1, y = MonthlyIncome))+geom_boxplot(color = "red")# Many Outliers
quantile(train$MonthlyIncome,c(0,.1,.2,.3,.4,.5,.6,.65,.7,.75,.8,.85,.9,.95,.99,.999,1))
train$MonthlyIncome[train$MonthlyIncome > 23000] <- 23000

# NumberOfOpenCreditLinesAndLoans
ggplot(train,aes(x = 1, y = NumberOfOpenCreditLinesAndLoans))+geom_boxplot(color = "red")# Many Outliers
quantile(train$NumberOfOpenCreditLinesAndLoans,c(0,.1,.2,.3,.4,.5,.6,.65,.7,.75,.8,.85,.9,.95,.99,.999,1))
train[train$NumberOfOpenCreditLinesAndLoans>24,"NumberOfOpenCreditLinesAndLoans"] <- 24

# NumberOfTimes90DaysLate
ggplot(train,aes(x = 1, y = NumberOfTimes90DaysLate))+geom_boxplot(color = "red")# couple Outliers
quantile(train$NumberOfTimes90DaysLate,c(0,.1,.2,.3,.4,.5,.6,.65,.7,.75,.8,.85,.9,.95,.99,.999,1))
train$NumberOfTimes90DaysLate[train$NumberOfTimes90DaysLate > 3] <- 3

# NumberRealEstateLoansOrLines
ggplot(train,aes(x = 1, y = NumberRealEstateLoansOrLines))+geom_boxplot(color = "red")
quantile(train$NumberRealEstateLoansOrLines,c(0,.1,.2,.3,.4,.5,.6,.65,.7,.75,.8,.85,.9,.95,.99,.999,1))
train[train$NumberRealEstateLoansOrLines > 9,"NumberRealEstateLoansOrLines"] <- 9

# NumberOfTime60.89DaysPastDueNotWorse
ggplot(train,aes(x = 1, y = NumberOfTime60.89DaysPastDueNotWorse))+geom_boxplot(color = "red")
quantile(train$NumberOfTime60.89DaysPastDueNotWorse,c(0,.1,.2,.3,.4,.5,.6,.65,.7,.75,.8,.85,.9,.95,.99,.999,1))
train$NumberOfTime60.89DaysPastDueNotWorse[train$NumberOfTime60.89DaysPastDueNotWorse > 2] <- 2

# NumberOfDependents
ggplot(train,aes(x = 1, y = NumberOfDependents))+geom_boxplot(color = "red")
quantile(train$NumberOfDependents,c(0,.1,.2,.3,.4,.5,.6,.65,.7,.75,.8,.85,.9,.95,.99,.999,1))
train[train$NumberOfDependents > 6,"NumberOfDependents"] <- 6

#.................................................................................................................#

# Converting response variable to factor
train$SeriousDlqin2yrs <- as.factor(train$SeriousDlqin2yrs)

#.....................................Dealing with imbalanced data...............................................#

prop.table(table(train$SeriousDlqin2yrs)) 
# Only 6.68% of positive caswes(default) and 93.3% of negative cases


# Oversampling
data_balanced_over <- ovun.sample(SeriousDlqin2yrs~., data = train,method = "over", N = 279948)$data
table(data_balanced_over$SeriousDlqin2yrs)
#Partition train into ntrain and ntest
set.seed(123)
trainIndex <- createDataPartition(data_balanced_over$SeriousDlqin2yrs,times = 1,p = 0.8,list = FALSE)
train_balanced_over <- data_balanced_over[trainIndex,]
test_balanced_over <- data_balanced_over[-trainIndex,]


# Undersampling
data_balanced_under <- ovun.sample(SeriousDlqin2yrs~., data = train,method = "under", N = 20052)$data
table(data_balanced_under$SeriousDlqin2yrs)
set.seed(123)
trainIndex1 <- createDataPartition(data_balanced_under$SeriousDlqin2yrs,times = 1,p = 0.8,list = FALSE)
train_balanced_under <- data_balanced_under[trainIndex1,]
test_balanced_under <- data_balanced_under[-trainIndex1,]


# Both
data_balanced_both <- ovun.sample(SeriousDlqin2yrs~., data = train,method = "both", p = 0.5)$data
table(data_balanced_both$SeriousDlqin2yrs)
set.seed(5)
trainIndex2 <- createDataPartition(data_balanced_both$SeriousDlqin2yrs,times = 1,p = 0.8,list = FALSE)
train_balanced_both <- data_balanced_both[trainIndex2,]
test_balanced_both <- data_balanced_both[-trainIndex2,]

# ROSE
data.rose <- ROSE(SeriousDlqin2yrs~., data = train,seed = 1)$data
table(data.rose$SeriousDlqin2yrs)
set.seed(6)
trainIndex3 <- createDataPartition(data.rose$SeriousDlqin2yrs,times = 1,p = 0.8,list = FALSE)
train_balanced_rose <- data.rose[trainIndex3,]
test_balanced_rose <- data.rose[-trainIndex3,]



#..........................................Logistic Regression...................................................#

# Logistic Regression for Over sampled data
log_model_over <- glm(SeriousDlqin2yrs~.,family = "binomial",data = train_balanced_over)
log_pred_over <- predict(log_model_over,newdata = test_balanced_over[,-1],type = "response")
log_prob_pred_over <- ifelse(log_pred_over > 0.5,1,0)
confusionMatrix(as.factor(log_prob_pred_over),test_balanced_over$SeriousDlqin2yrs,positive = '1')
roc.curve(test_balanced_over$SeriousDlqin2yrs,log_prob_pred_over)
# Sensitivity = 0.739 i.e. 73.9% accuracy in predicting the defaults(1)
# Specificity = 0.81 i.e. 81% accuracy in predicting non defaults(0)
# Overall Accuracy = 0.7751

# Logistic Regression for Under sampled data
log_model_under <- glm(SeriousDlqin2yrs~.,family = "binomial",data = train_balanced_under)
log_pred_under <- predict(log_model_under,newdata = test_balanced_under[,-1],type = "response")
log_prob_pred_under <- ifelse(log_pred_under > 0.5,1,0)
confusionMatrix(as.factor(log_prob_pred_under),test_balanced_under$SeriousDlqin2yrs,positive = '1')
# Sensitivity = 0.754 i.e. 75.4% accuracy in predicting the defaults(1)
# Specificity = 0.80 i.e. 80% accuracy in predicting non defaults(0)
# Overall Accuracy = 0.7803
roc.curve(test_balanced_under$SeriousDlqin2yrs,log_prob_pred_under)

# Logistic Regression for Both sampled data
log_model_both <- glm(SeriousDlqin2yrs~.,family = "binomial",data = train_balanced_both)
log_pred_both <- predict(log_model_both,newdata = test_balanced_both[,-1],type = "response")
log_pro_pred_both <- ifelse(log_pred_both > 0.5,1,0)
confusionMatrix(as.factor(log_pro_pred_both),test_balanced_both$SeriousDlqin2yrs,positive = '1')
# Sensitivity = 0.7415 i.e. 74.1% accuracy in predicting the defaults(1)
# Specificity = 0.8085 i.e. 80.8% accuracy in predicting non defaults(0)
# Overall Accuracy = 0.775

# Logistic Regression for ROSE sampled data
log_model_rose <- glm(SeriousDlqin2yrs~.,family = "binomial",data = train_balanced_rose)
log_pred_rose <- predict(log_model_rose,newdata = test_balanced_rose[,-1],type = "response")
log_prob_pred_rose <- ifelse(log_pred_rose > 0.5,1,0)
confusionMatrix(as.factor(log_prob_pred_rose),test_balanced_rose$SeriousDlqin2yrs,positive = '1')
roc.curve(test_balanced_rose$SeriousDlqin2yrs,log_prob_pred_rose)
# Sensitivity = 0.7238 i.e. 72.3% accuracy in predicting the defaults(1)
# Specificity = 0.7948 i.e. 79.4% accuracy in predicting non defaults(0)
# Overall Accuracy = 0.7593



#.............................................Decision Tree.......................................................#
# Decision Tree for Undersampled data
dt_model_under <- rpart(SeriousDlqin2yrs~.,data = train_balanced_under,method = "class")
rpart.plot(dt_model_under)
dt_pred_under <- predict(dt_model_under,newdata = test_balanced_under[,-1],type = "prob")
caTools::colAUC(dt_pred_under[, 1], test_balanced_under$SeriousDlqin2yrs,plotROC = TRUE)
#AUC: 0.7608721


#............................................Random Forest........................................................#

# Random forest for Oversampled data 
rf_model_over <- train(SeriousDlqin2yrs~., data = train_balanced_over,
                        method = "ranger", 
                        trControl = trainControl(method = "cv",number = 5,verboseIter = TRUE))
plot(rf_model_over)
rf_pred_over <- predict(rf_model_over,newdata = test_balanced_over[,-1])
roc.curve(rf_pred_over,test_balanced_over$SeriousDlqin2yrs) # AUC 0.985
confusionMatrix(rf_pred_over,test_balanced_over$SeriousDlqin2yrs,positive = '1')
# Sensitivity = 0.9997 i.e. 99.97% accuracy in predicting the defaults(1)
# Specificity = 0.9696 i.e. 96.96% accuracy in predicting non defaults(0)
# Overall Accuracy = 0.9847

# Random forest for Undersampled data 
rf_model_under <- train(SeriousDlqin2yrs~., data = train_balanced_under,
                        method = "ranger", 
                        trControl = trainControl(method = "cv",number = 5,verboseIter = TRUE))
plot(rf_model_under)
rf_pred_under <- predict(rf_model_under,newdata = test_balanced_under[,-1])
roc.curve(rf_pred_under,test_balanced_under$SeriousDlqin2yrs)# AUC 0.786
confusionMatrix(rf_pred_under,test_balanced_under$SeriousDlqin2yrs,positive = '1')
# Sensitivity = 0.7805 i.e. 78% accuracy in predicting the defaults(1)
# Specificity = 0.7920 i.e. 79.2% accuracy in predicting non defaults(0)
# Overall Accuracy = 0.7863

# Random forest for both sampled data 
rf_model_both <- train(SeriousDlqin2yrs~., data = train_balanced_both,
                       method = "ranger", 
                       trControl = trainControl(method = "cv",number = 5,verboseIter = TRUE))

plot(rf_model_both)
rf_pred_both <- predict(rf_model_both,newdata = test_balanced_both[,-1])
roc.curve(rf_pred_both,test_balanced_both$SeriousDlqin2yrs)# AUC 0.978
confusionMatrix(rf_pred_both,test_balanced_both$SeriousDlqin2yrs,positive = '1')
# Sensitivity = 0.9988 i.e. accuracy in predicting the defaults(1)
# Specificity = 0.9559 i.e. accuracy in predicting non defaults(0)
# Overall Accuracy = 0.9773

# Random forest for ROSE sampled data 

# In order to get the class probabilities, I had to change the levels and SeriousDlqin2yrs(0/1) to yes/no
levels(train_balanced_rose$SeriousDlqin2yrs)
levels(train_balanced_rose$SeriousDlqin2yrs)[levels(train_balanced_rose$SeriousDlqin2yrs)=="0"] <- "no"
levels(train_balanced_rose$SeriousDlqin2yrs)[levels(train_balanced_rose$SeriousDlqin2yrs)=="1"] <- "yes"
train_balanced_rose$SeriousDlqin2yrs[train_balanced_rose$SeriousDlqin2yrs==0]<-"no"
train_balanced_rose$SeriousDlqin2yrs[train_balanced_rose$SeriousDlqin2yrs==1]<-"yes"

levels(test_balanced_rose$SeriousDlqin2yrs)
levels(test_balanced_rose$SeriousDlqin2yrs)[levels(test_balanced_rose$SeriousDlqin2yrs)=="0"] <- "no"
levels(test_balanced_rose$SeriousDlqin2yrs)[levels(test_balanced_rose$SeriousDlqin2yrs)=="1"] <- "yes"
test_balanced_rose$SeriousDlqin2yrs[test_balanced_rose$SeriousDlqin2yrs==0]<-"no"
test_balanced_rose$SeriousDlqin2yrs[test_balanced_rose$SeriousDlqin2yrs==1]<-"yes"

rf_model_rose <- train(SeriousDlqin2yrs~., data = train_balanced_rose,
                       method = "ranger", 
                       trControl = trainControl(method = "cv",number = 5,verboseIter = TRUE,classProbs = TRUE))
plot(rf_model_rose)
rf_pred_rose <- predict(rf_model_rose,newdata = test_balanced_rose[,-1],'prob')
caTools::colAUC(rf_pred_rose[, 1], test_balanced_rose$SeriousDlqin2yrs,plotROC = TRUE)
# AUC 0.9760488
confusionMatrix(predict(rf_model_rose,newdata = test_balanced_rose[,-1]),test_balanced_rose$SeriousDlqin2yrs,positive = 'yes')
# Sensitivity = 0.9170 i.e. accuracy in predicting the defaults(1)
# Specificity = 0.9325 i.e. accuracy in predicting non defaults(0)
# Overall Accuracy = 0.9247

#...............................................Submission........................................................#

# Random Forest
probabilities <- predict(rf_model_rose,newdata = test[,-1],'prob')
output<-probabilities[,2]
submissionRF <- cbind(1:101503, output)
colnames(submissionRF) <- c("Id", "probability")
write.table(submissionRF, "20180730_RF_rose.csv", sep = ",", dec = ".", row.names = F)

# Logistic Regression
probabilities_log <- predict(log_model_under,newdata = test[,-1],type = "response")
submissionLog <- cbind(1:101503, probabilities_log)
colnames(submissionLog) <- c("Id", "probability")
write.table(submissionRF, "20180730_LGR.csv", sep = ",", dec = ".", row.names = F)

#Decision Tree
probabilities_dt <- predict(dt_model_under,newdata = test[,-1],type = "prob")
submissiondt <- cbind(1:101503, probabilities_dt[,2])
colnames(submissiondt) <- c("Id", "probability")
write.table(submissiondt, "20180730_DT.csv", sep = ",", dec = ".", row.names = F)
