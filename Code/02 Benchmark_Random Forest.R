library("visdat")
library("naniar")
library("caTools")
library("randomForest")
library("EMP")
library("ROCR")
library("pROC")
library("stats")
library("ROSE")
library("splitTools")
library("readxl")

# import data
data = read.csv("/Users/helefei/Documents/NUS/Modules/Sem3/ST5188/Telco Churn Dataset/IBM/IBM.csv")
data = data[,-1]

## any missing data？
vis_dat(data)
vis_miss(data)
gg_miss_var(data)
miss_case_table(data)

### which rows have missing data
missing_row = which(rowSums(is.na(data))!=0)

### delete these rows
data = data[-missing_row,]

# split the data into train and test (stratified sampling)
set.seed(5188)
split = sample.split(data$Churn.Label,SplitRatio = .7)
train_data = subset(data,split == TRUE)
test_data  = subset(data,split == FALSE)
  
# Random Forest
# select the optimal $mtry$ (no. of variables in each decision tree) using leave-one-out cv

set.seed(5188)
mtry_auc = matrix(NA, nrow = 2, ncol = 22)
folds <- create_folds(train_data$Churn.Label, k = 2)

for(i in 1:2){
    mtry_train = train_data[folds[[i]], ]
    mtry_test = train_data[-folds[[i]], ]
  for(j in 1:(length(names(train_data)))-1){
    mtry_model = randomForest(Churn.Label~., mtry_train, mtry=j)
    pred_score_test = predict(mtry_model,mtry_test,type = "prob")[,2]
    roc_mtry <- roc(mtry_test$Churn.Label, pred_score_test)
    mtry_auc[i,j] = roc_mtry$auc}}

mtry = which.max(apply(mtry_auc,2,mean))

# choose ntree, see if the error rate is stable
ntree_fit = randomForest(Churn.Label~., data=train_data, mtry=3, ntree=1000)
plot(ntree_fit)

# finalise the random forest model
SRF = randomForest(Churn.Label~., data=train_data, mtry=3, ntree=200)
SRF$confusion
importance(SRF)
varImpPlot(SRF)

# evaluate the model on the test set 
pred_score_SRF = predict(SRF,test_data,type = "prob")[,2]

# ROC
roc_SRF <- roc(test_data$Churn.Label, pred_score_SRF)
plot(roc_SRF,
     legacy.axes = TRUE,
     main="ROC curve",
     thresholds="best",  
     print.thres="best")

# optimal threshold
roc_result <- coords(roc_SRF, "best")
roc_result$threshold

# AUC 
roc_SRF$auc

# F1 socre 
table = cbind(test_data$Churn.Label,pred_score_SRF)
TP <- dim(table[as.numeric(test_data$Churn.Label)==2 & pred_score_SRF > roc_result$threshold, ])[1]
FP <- dim(table[as.numeric(test_data$Churn.Label)==1 & pred_score_SRF > roc_result$threshold, ])[1]
TN <- dim(table[as.numeric(test_data$Churn.Label)==1 & pred_score_SRF <= roc_result$threshold, ])[1]
FN <- dim(table[as.numeric(test_data$Churn.Label)==2 & pred_score_SRF <= roc_result$threshold, ])[1]

recall = TP/(TP+FN)
precision = TP/(TP+FP)

F1 = 2*recall*precision/(recall+precision)
F1

# EMPC (See:help(empChurn))
empc_SRF = empChurn(pred_score_SRF,test_data$Churn.Label, alpha = 6, beta = 8, clv = 200, d = 10, f = 1)
empc_SRF$EMP

# Accuracy 
Acc = (TP+TN)/(TP+TN+FP+FN)

Random_Forest = c(roc_SRF$auc,F1,precision,recall,empc_SRF$EMP,Acc)
summary = as.data.frame(Random_Forest,nrow = 6,row.names = c("AUC","F1-score","Precision","Recall","EMPC","Accuracy"))

