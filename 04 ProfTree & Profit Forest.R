library(proftree)
library(EMP)
library(splitTools)
library(DMwR2)
library(pROC)
library(doParallel)
library(tidyr)
library(plyr)
library(dplyr)
library(caTools)
library(ROSE)
library(ROCR)

# Data Preprocessing -------------------------------------------
# import data
telco=read.csv('/Users/matthewgalois/Desktop/IBM new.csv')
telco=na.omit(telco)

# as.factor
cols <- c('Gender','SeniorCitizen','Partner', 'Dependents','TenureMonths','PhoneService','MultipleLines','InternetService','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','Contract','PaperlessBilling','PaymentMethod','Churn')
telco[cols] <- lapply(telco[cols], as.factor)

# partition into training and testing set
tels <- partition(telco$Churn, p = c(train = 0.7, test = 0.3))
str(tels)
train <- telco[tels$train, ]
test <- telco[tels$test, ]
Lambda=0.1 

# Search for the best lambda
# lambda grid
L <- c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)
finalMatrix <- matrix(NA, nrow = 2, ncol = 21)
iteration = 1

# 5*2 folds
for (Lambda in L) {
  rep5 <- matrix(NA, nrow = 2, ncol = 5)
  for (j in 1:5) { 
    folds <- create_folds(train$Churn, k = 2)
    metric <- vector("numeric")
    for(i in 1:2){
      train_lambda <- train[folds[[i]], ]
      test_lambda <- train[-folds[[i]], ]
      ProfTree <- proftree(formula = Churn ~ ., #fit model
                           data = train_lambda,
                           control = proftree.control(lambda = Lambda,
                                                      seed = 2020,
                                                      verbose = TRUE,miniterations = 400,maxdepth = 3))
      scores.tree <- predict(ProfTree, newdata = test_lambda, type = "prob")[, 2] # evaluate model
      EMP <- empChurn(scores = scores.tree, classes = test_lambda$Churn)$EMP
      print(EMP)
      metric[i] <- EMP
      rep5[i, j] <- metric[i]
    }
  }
  cmean <- colMeans(rep5)
  avg_rep5 <- rbind(rep5,cmean)
  avg_lambda <- rowMeans(avg_rep5)[3]
  finalMatrix [1, iteration] <- Lambda
  finalMatrix [2, iteration] <- avg_lambda
  iteration <- iteration + 1
}
print(finalMatrix)

Lambda=0.1
# Fit ProfTree ----------------------------------------------
ProfTree <- proftree(formula = Churn ~ ., #fit model
                     data = train,
                     control = proftree.control(lambda = Lambda,
                                                seed =5188,
                                                verbose = TRUE,miniterations = 400,maxdepth = 5))
# calculate the scores and EMPC
scores.tree <- predict(ProfTree, newdata = test, type = "prob")[,2] # evaluate model
EMP.tree<- empChurn(scores = scores.tree, classes = test$Churn, alpha =6, beta = 8, clv = 200, d = 10, f = 1)$EMP
print(EMP.tree)
plot(ProfTree)

# ROC
roc_ProfTree <- roc(test$Churn, scores.tree)
plot(roc_ProfTree,
     legacy.axes = TRUE,
     main="ROC curve",
     thresholds="best",  
     print.thres="best")

# optimal threshold
roc_result_tree <- coords(roc_ProfTree, "best")
roc_result_tree$threshold

# AUC 
roc_ProfTree$auc

# F1 socre 
table.tree = cbind(test$Churn,scores.tree)
TP.tree <- dim(table.tree[as.numeric(test$Churn)==2 & scores.tree > roc_result_tree$threshold, ])[1]
FP.tree <- dim(table.tree[as.numeric(test$Churn)==1 & scores.tree > roc_result_tree$threshold, ])[1]
TN.tree <- dim(table.tree[as.numeric(test$Churn)==1 & scores.tree <= roc_result_tree$threshold, ])[1]
FN.tree <- dim(table.tree[as.numeric(test$Churn)==2 & scores.tree <= roc_result_tree$threshold, ])[1]

recall.tree = TP.tree/(TP.tree+FN.tree)
precision.tree = TP.tree/(TP.tree+FP.tree)

F1.tree = 2*recall.tree*precision.tree/(recall.tree+precision.tree)
F1.tree

# Accuracy 
Acc.tree = (TP.tree+TN.tree)/(TP.tree+TN.tree+FP.tree+FN.tree)

ProfTree = c(roc_ProfTree$auc,F1.tree,precision.tree,recall.tree,EMP.tree,Acc.tree)
summary.tree = as.data.frame(ProfTree,nrow = 6,row.names = c("AUC","F1-score","Precision","Recall","EMPC","Accuracy"))


# Fit Prof forest --------------------------------------------
# set the number of tree
treeCount=200
# Set progress bar
pb=txtProgressBar(min=0,max=treeCount,style = 3)
# set bootstrap ratio
bootstrapRatio=0.8
# Initialize list of n trees
output=vector("list", length = treeCount)
scores.forest=vector("list", length = treeCount)
EMP.iteration=vector(length = treeCount)
# name each element for readibility
names(output)=c(1:treeCount)
names(scores.forest)=c(1:treeCount)
# Build up random forests
for (i in 1:treeCount) {
  # Update progressBar bar
  setTxtProgressBar(pb,i)
  # Select the subset of predictors
  predictors=names(train)[names(train) != "Churn"]
  samplePredictorCount=floor(length(predictors)^0.5)
  sample.predictors=sample(predictors,samplePredictorCount)
  train.sample.predictors<- train[,sample.predictors]
  # form the new data set
  Churn=train[,"Churn"]
  train_sample=cbind(train.sample.predictors,Churn)
  # perform bootstrap selection of observations
  sample.data=train_sample[sample(nrow(train_sample),floor(nrow(train_sample)*bootstrapRatio), replace = TRUE),]
  # Fit the tree
  output[[i]] <- proftree(formula = Churn ~ ., #fit model
                       data = sample.data,
                       control = proftree.control(lambda = Lambda,
                                                  seed =5188,
                                                  verbose = TRUE,miniterations = 400,maxdepth = 5))
  scores.forest[[i]] <- predict(output[[i]], newdata = test, type = "prob")[,2] # evaluate model
  EMP.iteration[i] <- empChurn(scores = scores.forest[[i]], classes = test$Churn, alpha =6, beta = 8, clv = 200, d = 10, f = 1)$EMP
  # close progress bar
  close(pb)
  # return list of decision trees
}
# Calculate the EMPC of prof forest
scores.forest.sum=rep(0, length = length(scores.forest[[1]])) #
for (i in 1:treeCount) {
  scores.forest.sum=scores.forest.sum+scores.forest[[i]]
}

# simple mean voting
scores.forest.mean=scores.forest.sum/length(scores.forest.sum) 
EMP.forest.mean <- empChurn(scores = scores.forest.mean, classes = test$Churn, alpha =6, beta = 8, clv = 200, d = 10, f = 1)$EMP
print(EMP.forest.mean)

# weighted soft voting
weight=vector(length = treeCount)
for (i in 1:treeCount) {
  weight[i]=EMP.iteration[i]/sum(EMP.iteration)
}
scores.forest.weight=vector("list", length = length(scores.forest[[1]]))
for (i in 1:length(scores.forest[[1]])) {
  for (j in 1:treeCount) {
    scores.forest.weight[[i]]=c(scores.forest.weight[[i]],scores.forest[[j]][i])
  }
}
scores.forest.weighted.voting=vector(length = length(scores.forest[[1]]))
for (j in 1:length(scores.forest[[1]])) {
  scores.forest.weighted.voting[j]=scores.forest.weight[[j]]%*%weight
}
EMP.forest.weighted<- empChurn(scores = scores.forest.weighted.voting, classes = test$Churn, alpha =6, beta = 8, clv = 200, d = 10, f = 1)$EMP
print(EMP.forest.weighted)

# majority voting
scores.forest.hard=vector("list", length = treeCount)
for (i in 1:treeCount) {
  scores.forest.hard[[i]][which(scores.forest[[i]]>0.5)]<-1
  scores.forest.hard[[i]][which(scores.forest[[i]]<=0.5)]<-0
}

scores.forest.hardvoting=vector("list", length=length(scores.forest[[1]]))
for (i in 1:length(scores.forest.hard[[1]])) {
  for (j in 1:treeCount) {
    scores.forest.hardvoting[[i]]=c(scores.forest.hardvoting[[i]],scores.forest.hard[[j]][i])
  }
}

scores.forest.majority=vector(length = length(scores.forest[[1]]))
for (j in 1:length(scores.forest.hard[[1]])) {
  scores.forest.majority[j]=as.numeric(names(which.max(table(scores.forest.hardvoting[[j]]))))
}
EMP.forest.majority <- empChurn(scores = scores.forest.majority, classes = test$Churn, alpha =6, beta = 8, clv = 200, d = 10, f = 1)$EMP
print(EMP.forest.majority)

# ROC
roc_ProfForest <- roc(test$Churn, scores.forest.weighted.voting)
plot(roc_ProfForest,
     legacy.axes = TRUE,
     main="ROC curve",
     thresholds="best",  
     print.thres="best")

# optimal threshold
roc_result_forest <- coords(roc_ProfForest, "best")
roc_result_forest$threshold

# AUC 
roc_ProfForest$auc

# F1 socre 
table.forest = cbind(test$Churn,scores.forest.weighted.voting)
TP.forest <- dim(table.forest[as.numeric(test$Churn)==2 & scores.forest.weighted.voting > roc_result_forest$threshold, ])[1]
FP.forest <- dim(table.forest[as.numeric(test$Churn)==1 & scores.forest.weighted.voting > roc_result_forest$threshold, ])[1]
TN.forest <- dim(table.forest[as.numeric(test$Churn)==1 & scores.forest.weighted.voting <= roc_result_forest$threshold, ])[1]
FN.forest <- dim(table.forest[as.numeric(test$Churn)==2 & scores.forest.weighted.voting <= roc_result_forest$threshold, ])[1]

recall.forest = TP.forest/(TP.forest+FN.forest)
precision.forest = TP.forest/(TP.forest+FP.forest)

F1.forest = 2*recall.forest*precision.forest/(recall.forest+precision.forest)
F1.forest

# Accuracy 
Acc.forest = (TP.forest+TN.forest)/(TP.forest+TN.forest+FP.forest+FN.forest)

ProfForest = c(roc_ProfForest$auc,F1.forest,precision.forest,recall.forest,EMP.forest.weighted,Acc.forest)
summary.forest = as.data.frame(ProfForest,nrow = 6,row.names = c("AUC","F1-score","Precision","Recall","EMPC","Accuracy"))
