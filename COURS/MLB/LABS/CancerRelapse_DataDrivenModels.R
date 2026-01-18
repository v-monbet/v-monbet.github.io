# ===================================================
#      Prediction of a categorical variable
# ===================================================
library(class)
library(rpart)
library(pROC) # ROC curve



# Cancer Relaps dataset
X.all = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct",skip=2,header=TRUE,sep = "\t")
label = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls",skip=2,header=FALSE,sep = " ")



X.desc = X.all[,1]
X = t(X.all[,-c(1,2)])

y = factor(label,levels=c(0,1))

# =======================================================
train_test_split <- function (X,y,test_size=.25,random_state=NULL){
  # Extraction of a train and a test datasets.
  #
  # Inputs : X, y : data
  #          test_size : a fraction (<1) of the total set or a number of samples (integer) ; default 0.25
  #          random_state : if equal to an interger, it fixes the random seed ; defaut NULL
  # Outputs : X_train, X_test, y_train, y_test
  #
  n = nrow(X)
  if (test_size>1){test_size=test_size/n}
  if (!is.null(random_state)){set.seed(random_state)}
  itest=sample(1:n,round(n*test_size))
  itrain=setdiff(1:n,itest)
  Xtrain=X[itrain,]
  Xtest=X[itest,]
  ytrain=y[itrain]
  ytest=y[itest]
  return(list(X_train=Xtrain,X_test=Xtest,y_train=ytrain,y_test=ytest))
}

# =======================================================
# Function to find the genes with the highest SNR
genes_SNR = function(X,y){
  # - Inputs
  # X = dataset
  # y = labels (a boolean or factor variable)
  # - Output
  # SNR
  labels = unique(y)
  n = dim(X)[1]
  p = dim(X)[2]
  K = length(labels)
  
  means = matrix(0,length(labels),dim(X)[2])
  std = apply(X,2,sd)
  
  for (k in 1:K){means[k,] = apply(X[y==labels[k],],2,mean)}
  SNR = apply(matrix(abs(apply(means,2,diff)),K-1,p),2,max)/std
  return(SNR)
}

# =======================================================

# ==============================================================================
# Nearest neighbors algorithm for classification
B = 30 # number of samples for the cross validation
klist=c(1,3,5,10,20)
err_knn = matrix(0,B,length(klist))
prob_knn = NULL
ytest_knn = NULL
for (b in 1:B){
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_knn = c(ytest_knn,y_test)
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  # find the "best" genes
  SNR = genes_SNR(X_train,y_train)
  keep = sort.int(SNR,index.return=TRUE,decreasing=TRUE)$ix[1:30]
  # learning knn model with various values of the number of neighbors
  prob_tmp = NULL
  for (k in 1:length(klist)){
    res = knn(X_train[,keep],X_test[,keep],y_train,k=klist[k],prob=TRUE)
    err_knn[b,k] = mean((y_test!=res)) # compute MSE
    prob_tmp = cbind(prob_tmp,attributes(res)$prob)
  }
  prob_knn = rbind(prob_knn,prob_tmp)
  
}  
err_knn_all = apply(err_knn,2,mean) # global error
par(mfrow=c(1,2)) # Two subfigures
plot(klist,err_knn_all,pch=20) # error for each value of k
grid()

k0 = which.min(err_knn_all) # to find for which values of k the error is min
res_roc = roc(ytest_knn,prob_knn[,k0])
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))



# ==============================================================================
# Decision tree


B = 30 # number of samples for the cross validation

err_tree = NULL
prob_tree = NULL
ytest_tree = NULL
for (b in 1:B){
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_tree = c(ytest_tree,y_test)
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  data_train = data.frame(X_train,y_train)
  data_test = data.frame(X_test,y_test)
  
  # learning decision tree mode
  tree = rpart(y_train~.,data=data_train)
  y_pred= predict(tree,data_test,type="class") 
  err_tree[b] = mean((y_test!=y_pred)) 
  prob_tree = c(prob_tree,predict(tree,data_test,type="prob")[,2])
}  
err_tree_all = mean(err_tree) # global error
print(paste("Mean classification error, tree = ",round(err_tree_all,2)))

res_roc = roc(ytest_tree,prob_tree)
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))

#"""
#
#Try knn and/or tree on PCA (or other smaller number of predictor)
#
#"""
