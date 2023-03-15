library("visdat")
library("naniar")
library("caTools")
library("partykit")

# import data
data = read.csv("/Users/helefei/Documents/NUS/Modules/Sem3/ST5188/Telco Churn Dataset/IBM/IBM new.csv")
data = data[,-1]

# any missing data？
vis_dat(data)
vis_miss(data)
gg_miss_var(data)
miss_case_table(data)

# which rows have missing data
missing_row = which(rowSums(is.na(data))!=0)

# delete these rows
data = data[-missing_row,]

# split the data into train and test (stratified sampling)
set.seed(5188)
split = sample.split(data$Churn.Label,SplitRatio = .7)
train_data = subset(data,split == TRUE)
test_data  = subset(data,split == FALSE)


# CForest
# https://discuss.analyticsvidhya.com/t/how-to-plot-a-sample-tree-from-random-forest-in-r/2800/2
# https://search.r-project.org/CRAN/refmans/partykit/html/cforest.html
# https://www.geeksforgeeks.org/conditional-inference-trees-in-r-programming/

# Standard Conditional Forest
# set max_depth = 3 and 5 respectively
set.seed(5188)
mtry_auc = matrix(NA, nrow = 2, ncol = 22)
folds <- create_folds(train_data$Churn.Label, k = 2)

for(i in 1:2){
  mtry_train = train_data[folds[[i]], ]
  mtry_test = train_data[-folds[[i]], ]
  for(j in 1:(length(names(train_data)))-1){
    mtry_model = cforest(Churn.Label~., mtry_train, control = ctree_control(maxdepth = 3),mtry=j)
    pred_score_test = predict(mtry_model,mtry_test,type = "prob")[,2]
    roc_mtry <- roc(mtry_test$Churn.Label, pred_score_test)
    mtry_auc[i,j] = roc_mtry$auc}}

mtry = which.max(apply(mtry_auc,2,mean))
SCF = cforest(Churn.Label~., data=train_data, mtry = mtry,control = ctree_control(maxdepth = 3))

# prediction on test set
pred_score_SCF = predict(SCF,test_data,type = "prob")[,2]

# ROC
roc_SCF <- roc(test_data$Churn.Label, pred_score_SCF)
plot(roc_SCF,
     legacy.axes = TRUE,
     main="ROC curve of SCF",
     thresholds="best",  
     print.thres="best")

# optimal threshold
roc_result <- coords(roc_SCF, "best")
roc_result$threshold

# AUC 
roc_SCF$auc

# F1 socre 
table = cbind(test_data$Churn.Label,pred_score_SCF)
TP <- dim(table[as.numeric(test_data$Churn.Label)==2 & pred_score_SCF > roc_result$threshold, ])[1]
FP <- dim(table[as.numeric(test_data$Churn.Label)==1 & pred_score_SCF > roc_result$threshold, ])[1]
TN <- dim(table[as.numeric(test_data$Churn.Label)==1 & pred_score_SCF <= roc_result$threshold, ])[1]
FN <- dim(table[as.numeric(test_data$Churn.Label)==2 & pred_score_SCF <= roc_result$threshold, ])[1]

recall = TP/(TP+FN)
precision = TP/(TP+FP)

F1 = 2*recall*precision/(recall+precision)
F1

# EMPC (See:help(empChurn))
empc_SCF = empChurn(pred_score_SCF,test_data$Churn.Label, alpha = 6, beta = 8, clv = 200, d = 10, f = 1)
empc_SCF$EMP

# Accuracy 
Acc = (TP+TN)/(TP+TN+FP+FN)

CForest = c(roc_SCF$auc,F1,precision,recall,empc_SCF$EMP,Acc)
summary = as.data.frame(CForest,nrow = 6,row.names = c("AUC","F1-score","Precision","Recall","EMPC","Accuracy"))
