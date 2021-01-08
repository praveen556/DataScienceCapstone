# CARAVAN INSURANCE POLICY PREDICTION
# Capstone Project
# HarvardX: PH125.9x Data Science: Capstone
# R Code
# Author: Shilpa Susan Thomas
# Date: 07/05/2020



####################
# DATA PREPARATION #
####################



# Load the R packages that will be required to run the code for the project and will be useful 
# The packages are installed if they are missing so that the code can be easily reproduced 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")


# Caravan Insurance Data set
# Download the dataset from the author's Github account
# https://raw.githubusercontent.com/ShilpaSThomas/Caravan-Insurance-Capstone-Project/master/caravan-insurance-challenge.csv

urlfile =
  "https://raw.githubusercontent.com/ShilpaSThomas/Caravan-Insurance-Capstone-Project/master/caravan-insurance-challenge.csv"
CIdata <- read_csv(url(urlfile))
head(CIdata)


# Load some extra R packages that will be needed for analysis and visualisations
# The packages are installed if they are missing so that the code can be easily reproduced

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")


# Basic analysis
summary(CIdata)
names(CIdata)
str(CIdata)
levels(CIdata[,87]) # target variable has to be factor
dim(CIdata)


# Checking if there is missing data
number_rows <- nrow(CIdata)
complete_rows <- sum(complete.cases(CIdata))
number_rows == complete_rows #no missing data

# Change certain columns to factors
CIdata$MOSTYPE <- as.factor(CIdata$MOSTYPE)
CIdata$MGEMLEEF <- as.factor(CIdata$MGEMLEEF)
CIdata$MOSHOOFD <- as.factor(CIdata$MOSHOOFD)


# Rename target variable for easier understanding
CIdata <- CIdata %>% rename(Purchase = CARAVAN)

# Convert target variable to factor with yes and no
CIdata$Purchase <- factor(CIdata$Purchase,
                          levels = c(1,0),
                          labels = c("Yes", "No"))
str(CIdata$Purchase)


# Create training & test set 
# These will be used for testing models
# The same seed is set for reproducibility 

set.seed(1)

test_index <- createDataPartition(y = CIdata$Purchase, times = 1,
                                  p = 0.3, list = FALSE)
train_set <- CIdata[-test_index,]
test_set <- CIdata[test_index,]

nrow(train_set)
nrow(test_set)

# Remove ORIGIN variable from the train and test sets
train_set$ORIGIN <- NULL
test_set$ORIGIN <- NULL



#################################
# DATA EXPLORATION AND ANALYSIS #
#################################



# Checking insurance holders in the data set
train_set_Purchase <- train_set[c("Purchase")] %>% count(Purchase) %>% 
  mutate(prop = prop.table(n))
train_set_Purchase %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>%column_spec(3,border_right = T) %>%
  row_spec(0,bold=T) # very imbalanced dataset


# Purchase plot
train_set %>% ggplot(aes(x=train_set$Purchase)) + 
  geom_bar(fill = "blue") +
  xlab('Caravan Policy') + 
  ylab('Number of Customers') + 
  ggtitle("Purchase of Caravan Policy ") 
  


#Going by intuition, plots for certain variables where there are customers who got caravan insurance:



# Customer Type
Type <- table(train_set$MOSHOOFD[train_set$Purchase == "Yes"])
barplot(Type, col="green", 
        main = "Customer Types for Caravan Policy", 
        xlab = "Customer Type", ylab = "Number of customers")


# Sub customer type
subCustType <- data.frame(train_set$MOSTYPE,train_set$Purchase)
subCustType$train_set.MOSTYPE <- as.factor(subCustType$train_set.MOSTYPE)
subCustType$train_set.Purchase <- as.factor(subCustType$train_set.Purchase)
subCustType %>% filter(train_set.Purchase == "Yes") %>% 
  ggplot(aes(x=reorder(train_set.MOSTYPE, train_set.MOSTYPE,function(x)-length(x)))) + 
           geom_bar(fill = "blue") + 
  labs(x="Customer Subtype", y = "Number of customers")


# Age
Age <- table(train_set$MGEMLEEF[train_set$Purchase == "Yes"])
barplot(Age, col = "green", 
        main = "Average Age of Caravan Policy Holders", 
        xlab = "Age Group", ylab = "Number of customers")


# Household Size
train_set %>% filter(Purchase == "Yes") %>%
  ggplot(aes(MGEMOMV)) + 
  geom_bar(fill = "orange") +
  labs(x = "Average Household Size", y = "Number of Customers")


# Average Income
pie <- ggplot(train_set, aes(x = "", fill = factor(MINKGEM))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Average Income")
pie + coord_polar(theta = "y", start=0)  


# Purchasing Power Class
train_set$MKOOPKLA <- factor(train_set$MKOOPKLA)
train_set %>% filter(Purchase == "Yes") %>% 
  ggplot(aes(x=reorder(MKOOPKLA, MKOOPKLA,function(x)-length(x)))) + 
  geom_bar(fill = "orange") +
  labs(x= "Purchasing Power Class", y="Number of Customers")
train_set$MKOOPKLA <- as.numeric(train_set$MKOOPKLA)


# Number of car policies
carpolicy <- data.frame(train_set$APERSAUT,train_set$Purchase)
carpolicy$train_set.APERSAUT <- as.factor(carpolicy$train_set.APERSAUT)
carpolicy$train_set.Purchase <- as.factor(carpolicy$train_set.Purchase)
carpolicy %>% filter(train_set.Purchase == "Yes") %>% 
  ggplot(aes(x=reorder(train_set.APERSAUT, train_set.APERSAUT,function(x)-length(x)))) + 
  geom_bar(fill = "blue") + labs(x="Car Policies", y = "Number of customers")


# Contribution Private Insurance
priInsure <- data.frame(train_set$PWAPART,train_set$Purchase)
priInsure$train_set.PWAPART <- as.factor(priInsure$train_set.PWAPART)
priInsure$train_set.Purchase <- as.factor(subCustType$train_set.Purchase)
priInsure %>% filter(train_set.Purchase == "Yes") %>% 
  ggplot(aes(x=reorder(train_set.PWAPART, train_set.PWAPART,function(x)-length(x)))) + 
  geom_bar(fill = "green") + 
  labs(x="Contribution Private Insurance", y = "Number of customers")



# Preprocessing 

# Feature Selection 
# Recursive Feature Elimination method

# ensure the results are repeatable
set.seed(7)
# load the library
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


# define the control using a random forest selection function
rfFuncs$summary <- twoClassSummary
ctrl_rfe <- rfeControl(method = "cv",
                       number = 5,
                       verbose = TRUE,
                       functions = rfFuncs,
                       allowParallel = F)
# run the RFE algorithm
results <- rfe(train_set[,1:85], 
               train_set$Purchase, 
               sizes = c(1:85), 
               metric = "ROC",
               rfeControl = ctrl_rfe)

# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))    


# Removing predictors with very few non-unique values or close to zero variation
nzx <- nearZeroVar(train_set, names = TRUE)
nzx
nzx <- nzx[1:35]


# PCA to try reduce dimensionality
train_set1 <- train_set
train_set1 <- sapply( train_set1, as.numeric )
cormatrix <- cor(train_set1, use="pairwise.complete")
head(cormatrix)

pca <- prcomp(cormatrix[,1:85], center = TRUE, scale = TRUE)
dim(pca$rotation)
summary(pca)

plot(pca$sdev)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


# Stepwise Variable Selection

# full logistic regression model
full.model <- glm(Purchase ~., data = train_set, family = binomial)
coef(full.model)

# Load some extra R packages that will be needed for analysis and visualisations
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")

# select the most contributive variables
step.model <- full.model %>% stepAIC(direction = "both", trace = FALSE)
summary(step.model)
step.model$anova
coef(step.model)
stepwise_train_set <- train_set[, c("MGODOV", "MGODGE", "MOPLMIDD", "MOPLLAAG", 
                                    "MRELGE","MBERZELF", "MBERBOER", "MBERMIDD", 
                                    "MHHUUR", "MFWEKIND", "MAUT1", "MINK123M", 
                                    "PWAPART", "PPERSAUT", "PLEVEN", "PPERSONG", 
                                    "PGEZONG", "PWAOREG", "PBRAND", "PZEILPL", 
                                    "PPLEZIER", "PINBOED", "PBYSTAND", "AWAPART", 
                                    "AWALAND", "ABESAUT", "AWERKT", "ALEVEN", 
                                    "APERSONG", "AGEZONG", "ABRAND", "AFIETS", 
                                    "AINBOED", "ABYSTAND", "MOSHOOFD", "Purchase")]


# Check correlation in subgroups:
group1 <- c("MOSTYPE", "MAANTHUI", "MGEMOMV", "MGEMLEEF", "MOSHOOFD", "Purchase")
religion <- c("MGODRK", "MGODPR", "MGODOV", "MGODGE", "Purchase")
marriage <- c("MRELGE","MRELSA", "MRELOV", "MFALLEEN", "Purchase")
children <- c("MFGEKIND", "MFWEKIND", "Purchase")
education <- c("MOPLHOOG", "MOPLMIDD", "MOPLLAAG", "Purchase")
socialclass <- c("MBERHOOG", "MBERZELF", "MBERBOER", "MBERMIDD", "MBERARBG",
                   "MBERARBO", "MSKA","MSKB1", "MSKB2", "MSKC", "MSKD", "Purchase")
home <- c("MHHUUR", "MHKOOP", "Purchase")
car <- c("MAUT1", "MAUT2", "MAUT0", "Purchase")
healthinsure <- c("MZFONDS", "MZPART", "Purchase")
income <- c("MINKM30", "MINK3045", "MINK4575", "MINK7512", "MINK123M", "MINKGEM", 
            "MKOOPKLA", "Purchase")


# Load some extra R packages that will be needed for analysis and visualisations
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")


cormatrix1 <- cor(train_set1[,group1], method = "spearman")
cormatrix1
corrplot(cormatrix1)
# customer main type and sub type are highly correlated
# so we will remove MOSTYPE as it is less correlated with Purchase

cormatrix2 <- cor(train_set1[,religion], method = "spearman")
cormatrix2
# all the religion variables are not much correlated with the target variable Purchase

cormatrix3 <- cor(train_set1[,marriage], method = "spearman")
cormatrix3
# only need one variable that says the number of married, MRELGE

cormatrix4 <- cor(train_set1[,children], method = "spearman")
cormatrix4
# only need one variable
# MFWEKIND will be used since it has higher correlation with Purchase

cormatrix5 <- cor(train_set1[,education], method = "spearman")
cormatrix5
# no high correlations between education classes so no removals

cormatrix6 <- cor(train_set1[,socialclass], method = "spearman")
cormatrix6
# no high correlations between social classes so no removals

cormatrix7 <- cor(train_set1[,home], method = "spearman")
cormatrix7
# renting a home or owning a home is highly correlated

cormatrix8 <- cor(train_set1[,car], method = "spearman")
cormatrix8
# only need one variable that says car ownership or not, MAUT1

cormatrix9 <- cor(train_set1[,healthinsure], method = "spearman")
cormatrix9
# haing private or public health insurance is highly correlated
# MZFONDS is removed

cormatrix10 <- cor(train_set1[,income], method = "spearman")
cormatrix10
# no high correlations between income levels


# train set with removal of highly correlated and unnecessary predictors
# removed from the set already gone through stepwise regression
train_set2 <- stepwise_train_set



#####################
# MODELLING PROCESS #
#####################



# USING DATA SET AFTER CHECKING FOR CORRELATIONS: train_set2


# Load some extra R packages that will be needed for the model
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")


# Create training & validation set from the original train data set
# These will be used for testing models
# The same seed is set for reproducibility 

set.seed(1)

test_index <- createDataPartition(y = train_set2$Purchase, times = 1,
                                  p = 0.3, list = FALSE)
training_set <- train_set2[-test_index,]
validation_set <- train_set2[test_index,]

nrow(training_set)
nrow(validation_set)
mean(training_set$Purchase == "Yes")



# Logistic Regression

training_glm <- training_set

set.seed(3) # for reproducibility

control <- trainControl(summaryFunction= twoClassSummary, classProbs = TRUE)

train_glm <- train(Purchase ~ ., 
                   method = "glm",
                   family = binomial,
                   data = training_glm,
                   trControl = control,
                   metric = "ROC")

train_glm
y_hat_glm <- predict(train_glm, validation_set)
summary(y_hat_glm)
cm_glm <- confusionMatrix(y_hat_glm, validation_set$Purchase)
cm_glm$table

# Measures of the model
Accuracy_glm <- confusionMatrix(y_hat_glm, validation_set$Purchase)$
  overall[["Accuracy"]]
Sensitivity_glm <- confusionMatrix(y_hat_glm, validation_set$Purchase)$
  byClass[["Sensitivity"]]
Specificity_glm <- confusionMatrix(y_hat_glm, validation_set$Purchase)$
  byClass[["Specificity"]]
F1_glm <- confusionMatrix(y_hat_glm, validation_set$Purchase)$
  byClass[["F1"]]

# Put the measures in a table
Results <- data.frame(Method = "Generalised Linear Model", 
                      Accuracy = Accuracy_glm, 
                      Sensitivity = Sensitivity_glm, 
                      Specificity = Specificity_glm, 
                      F1_score = F1_glm)

Results %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>% column_spec(5, border_right = T ) %>%
  row_spec(0,bold=T)

# GLM ROC Curve
# Load some extra R packages that will be needed for the model
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")

glm_pred <- predict(train_glm, validation_set, type = "prob")[,1]

glmROC <- roc(validation_set$Purchase, glm_pred,
              plot = TRUE, print.auc = TRUE, 
              main = "Performance of Logistic Regression")



# K Nearest Neighbour

# Load some extra R packages that will be needed for the model
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")

training_knn <- training_set %>% mutate_all(as.numeric)
validation_knn <- validation_set %>% mutate_all(as.numeric)

#Normalising each variable so they are scaled
normalise <- function(x)
{    
  u <- mean(x)    
  o <- sd(x)    
  (x - u)/(o)
}

train_knn_normalised <- apply(training_knn, 2, normalise)
val_knn_normalised <- apply(validation_knn, 2, normalise)

train_input <- as.matrix(train_knn_normalised[,-36])
train_output <- as.vector(train_knn_normalised[,36])
test_input <- as.matrix(val_knn_normalised[,-36])
test_output <- as.matrix(val_knn_normalised[,36])

set.seed(5) # for reproducibility

#pick the k in knn using cross validation

kmax <- 35
ER1 <- rep(0, kmax)
ER2 <- rep(0, kmax)
for (k in 1:kmax){
  prediction <- knn(train_input, train_input, train_output, k=k)
  prediction2 <- knn(train_input, test_input, train_output, k=k)
  
  #Confusion Matrix For Test Data
  cmtest <- table(prediction2, val_knn_normalised[,"Purchase"])
  
  #Error Rate on the test sample
  ER2[k] <- (cmtest[1,2]+cmtest[2,1])/sum(cmtest)
}


# Minimum Validation Error k
plot(1:kmax, ER2, xlab = "k", ylab = "Error Rate")
best_k <- which.min(ER2)
best_k

fit_knn <- knn(train_input, test_input, train_output, k= best_k)
actual <- ifelse(val_knn_normalised[,"Purchase"]> 0, 1, 0)
predicted <- ifelse(as.numeric(fit_knn)>0, 0, 1)
u <- union(predicted, actual)
t <- table(factor(predicted, u), factor(actual, u))
cm_test <- confusionMatrix(t)
cm_test$table

# Measures for the model
Accuracy_knn <- cm_test$overall["Accuracy"]
Sensitivity_knn <- cm_test$byClass["Sensitivity"]
Specificity_knn <- cm_test$byClass["Specificity"]
F1_knn <- cm_test$byClass["F1"]


# Put the measures in the results table produced earlier
Results <- bind_rows(Results, data_frame(Method="K Nearest Neighbour", 
                                         Accuracy = Accuracy_knn, 
                                         Sensitivity = Sensitivity_knn, 
                                         Specificity = Specificity_knn, 
                                         F1_score = F1_knn))

Results %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>% column_spec(5, border_right = T ) %>%
  row_spec(0,bold=T)

#KNN ROC curve
knn_pred <- knn(train_input, test_input, train_output, 
                k= best_k, prob = TRUE)
prob <- attr(knn_pred, "prob")
knnROC <- roc(validation_set$Purchase, prob,
              smoothed = TRUE,
              plot=TRUE, print.auc=TRUE, show.thres=TRUE,
              main = "Performance of KNN")



# Decision Trees

training_rpart <- training_set

# Load some extra R packages that will be needed for the model
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

set.seed(7) # for reproducibility

training_set$Purchase = as.factor(training_set$Purchase)

train_rpart <- rpart(formula=Purchase ~ .,
                     data = training_rpart,
                     control = rpart.control(minsplit = 26, cp = 0.001),
                     method = "class")

print(train_rpart)
train_rpart$variable.importance
rpart.plot(train_rpart)

y_hat_rpart <- predict(train_rpart, validation_set, type = "class")
cm_rpart <- confusionMatrix(y_hat_rpart, validation_set$Purchase)
cm_rpart$table

# Measures for the model
Accuracy_rpart <- confusionMatrix(y_hat_rpart, validation_set$Purchase)$
  overall["Accuracy"]
Sensitivity_rpart <- confusionMatrix(y_hat_rpart, validation_set$Purchase)$
  byClass["Sensitivity"]
Specificity_rpart <- confusionMatrix(y_hat_rpart, validation_set$Purchase)$
  byClass["Specificity"]
F1_rpart <- confusionMatrix(y_hat_rpart, validation_set$Purchase)$
  byClass["F1"]

# Put measures in the results table
Results <- bind_rows(Results, data_frame(Method="Decision Tree", 
                                         Accuracy = Accuracy_rpart, 
                                         Sensitivity = Sensitivity_rpart, 
                                         Specificity = Specificity_rpart, 
                                         F1_score = F1_rpart))

Results %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>% column_spec(5, border_right = T ) %>%
  row_spec(0,bold=T)

# Decision Tree ROC curve
rpart_pred <- predict(train_rpart, validation_set, type = "prob")[,1]

rpartROC <- roc(validation_set$Purchase, rpart_pred,
              smoothed = TRUE,
              plot=TRUE, print.auc=TRUE, show.thres=TRUE,
              main = "Performance of Decision Tree")



# Random Forest

training_rf <- training_set

# Load extra R package that will be needed for the model
# The packages are installed if they are missing so that the code can be easily reproduced 
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

set.seed(9) # for reproducibility

train_rf <- randomForest(Purchase ~ ., data = training_set)

varImp(train_rf) # feature importance
varImpPlot(train_rf, cex = 0.7)

cm_rf <- confusionMatrix(predict(train_rf, validation_set),
                         validation_set$Purchase)
cm_rf$table                      

# Measures for the model
Accuracy_rf <- confusionMatrix(predict(train_rf, validation_set),
                validation_set$Purchase)$overall["Accuracy"]
Sensitivity_rf <- confusionMatrix(predict(train_rf, validation_set),
                                  validation_set$Purchase)$byClass["Sensitivity"]

Specificity_rf <- confusionMatrix(predict(train_rf, validation_set),
                                  validation_set$Purchase)$byClass["Specificity"]
F1_rf <- confusionMatrix(predict(train_rf, validation_set),
                                 validation_set$Purchase)$byClass["F1"]

# Put measures in the results table
Results <- bind_rows(Results, data_frame(Method="Random Forest", 
                                         Accuracy = Accuracy_rf, 
                                         Sensitivity = Sensitivity_rf, 
                                         Specificity = Specificity_rf, 
                                         F1_score = F1_rf))

Results %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>% column_spec(5,border_right = T ) %>%
  row_spec(0,bold=T)

# Random forest ROC curve
rf_pred <- predict(train_rf, validation_set, type = "prob")[,1]

rf2ROC <- roc(validation_set$Purchase, rf_pred,
              smoothed = TRUE,
              plot=TRUE, print.auc=TRUE, show.thres=TRUE,
              main = "Performance of Random Forest")



# Regularised Random Forest

training_rrf <- training_set

set.seed(11) # for reproducibility

#pick the mtry in rf using cross validation

mtry <- seq(1, 33, 2)
rf <- map_df(mtry, function(m){
  
  fit_rf <- randomForest(Purchase ~ ., data = training_rrf, mtry = m)
  y_hat_rf <- predict(fit_rf, training_set, type = "class")
  cm_train <- confusionMatrix(y_hat_rf, training_set$Purchase)
  train_sens <- cm_train$byClass["Sensitivity"]
  y_hat_rf <- predict(fit_rf, validation_set, type = "class")
  cm_test <- confusionMatrix(y_hat_rf, validation_set$Purchase)
  test_acc <- cm_test$overall["Accuracy"]
  test_sens <- cm_test$byClass["Sensitivity"]
  test_spec <- cm_test$byClass["Specificity"]
  test_F1 <- cm_test$byClass["F1"]
  
  
  tibble(mtry = m, train = train_sens, test = test_sens, acc = test_acc, 
         spec = test_spec, F1 = test_F1)
})
rf

# Pick the mtry that maximises F1 using the estimates built on the test data
rf$mtry[which.max(rf$F1)]

# Measures for the model
Accuracy_rf2 <- rf$acc[which.max(rf$F1)]
Sensitivity_rf2 <- rf$test[which.max(rf$F1)]
Specificity_rf2 <- rf$spec[which.max(rf$F1)]
F1_rf2 <- rf$F1[which.max(rf$F1)]


# Put measures in the results table
Results <- bind_rows(Results, data_frame(Method="Random Forest with tuning mtry", 
                                         Accuracy = Accuracy_rf2, 
                                         Sensitivity = Sensitivity_rf2, 
                                         Specificity = Specificity_rf2, 
                                         F1_score = F1_rf2))

Results %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>% column_spec(5,border_right = T ) %>%
  row_spec(0,bold=T)


# Regularised Random forest ROC curve

train_rrf <- randomForest(Purchase ~ ., 
                          data = training_rrf, 
                          mtry = rf$mtry[which.max(rf$F1)])

rrf_pred <- predict(train_rrf, validation_set, type = "prob")[,1]

rrf2ROC <- roc(validation_set$Purchase, rrf_pred,
              smoothed = TRUE,
              plot=TRUE, print.auc=TRUE, show.thres=TRUE,
              main = "Performance of Random Forest")




###############
# FINAL MODEL #
###############



# Regularised random forest 

test_set <- test_set[, c("MGODOV", "MGODGE", "MOPLMIDD", "MOPLLAAG", 
                         "MRELGE","MBERZELF", "MBERBOER", "MBERMIDD", 
                         "MHHUUR", "MFWEKIND", "MAUT1", "MINK123M", 
                         "PWAPART", "PPERSAUT", "PLEVEN", "PPERSONG", 
                         "PGEZONG", "PWAOREG", "PBRAND", "PZEILPL", 
                         "PPLEZIER", "PINBOED", "PBYSTAND", "AWAPART", 
                         "AWALAND", "ABESAUT", "AWERKT", "ALEVEN", 
                         "APERSONG", "AGEZONG", "ABRAND", "AFIETS", 
                         "AINBOED", "ABYSTAND", "MOSHOOFD", "Purchase")]


set.seed(15) # for reproducibility

fit_rf <- randomForest(Purchase ~ .,
                       data = train_set2, 
                       mtry = rf$mtry[which.max(rf$F1)])

y_hat_rf <- predict(fit_rf, test_set, type = "class")
cm_test <- confusionMatrix(y_hat_rf, test_set$Purchase)
cm_test$table


# Measures for the model
final_Accuracy_rf <- cm_test$overall["Accuracy"]
final_Sensitivity_rf <- cm_test$byClass["Sensitivity"]
final_Specificity_rf <- cm_test$byClass["Specificity"]
final_F1_rf <- cm_test$byClass["F1"]


# Put measures in the results table
Final_Results <- data_frame(Method ="Random Forest with tuning mtry on test set", 
                            Accuracy = final_Accuracy_rf, 
                            Sensitivity = final_Sensitivity_rf, 
                            Specificity = final_Specificity_rf, 
                            F1_score = final_F1_rf)

Final_Results %>% knitr::kable() %>% kable_styling(position = 'center') %>%
  column_spec(1,border_left = T) %>% column_spec(5,border_right = T ) %>%
  row_spec(0,bold=T)

varImp(fit_rf)
varImpPlot(fit_rf, cex = 0.7)


#ROC curve

y_hat_rf2train <- predict(fit_rf, train_set2, type = "prob")[,1]
y_hat_rf2test <- predict(fit_rf, test_set, type = "prob")[,1]

rf2ROC <- roc(test_set$Purchase, y_hat_rf2test,
              smoothed = TRUE,
              ci=TRUE, ci.alpha=0.9, stratified=FALSE,
              plot=TRUE, print.auc=TRUE, show.thres=TRUE,
              main = "Performance of Regularised Random Forest on Test Data Set")
sens.ci <- ci.se(rf2ROC)
plot(sens.ci, type="shape", col="lightblue")



