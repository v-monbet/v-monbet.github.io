# ===================================================
#      Prediction of a categorical variable
# ===================================================
library(pROC)


# Cancer Relapse dataset
X.all = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct",skip=2,header=TRUE,sep = "\t")
label = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls",skip=2,header=FALSE,sep = " ")


#chemin = "~/Documents/ENSEIGNEMENT/MACHINE LEARNING/Master BMC/PROJETS/Ex_datasets/Ex2/"
#X.all = read.table(paste(chemin,"Ex2_breast_cancer_ER_train_set_for_other_methods.gct",sep=""),skip=2,header=TRUE,sep = "\t")
#label = read.table(paste(chemin,"Ex2_breast_cancer_ER_train_set_class_labels.cls",sep=""),skip=2,header=FALSE,sep = " ")

X.desc = X.all[,1]
X = t(X.all[,-c(1,2)])
colnames(X) = X.desc

y = factor(label,levels=c(0,1))


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
# adaboost =====================================================================
library(adabag) # boosting

B = 30 # number of samples for the cross validation

err_adab = NULL
prob_adab = NULL
ytest_adab = NULL
for (b in 1:B){
  print(paste("b =",b))
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_adab = c(ytest_adab,y_test)
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  data_train = data.frame(y_train,X_train)
  data_test = data.frame(y_test,X_test)
  colnames(data_train)[1] = "y"
  colnames(data_test)=colnames(data_train)
  
  # learning decision tree mode
  adaboost = boosting(y~.,data=data_train,mfinal=20,control=rpart.control(maxdepth=5, minsplit=5))
  y_pred= predict(adaboost,data_test,type="class") 
  err_adab[b] = mean((y_test!=y_pred)) 
  prob_adab = c(prob_adab,predict(adaboost,data_test)$prob[,2])
}  
err_adab_all = mean(err_adab) # global error
print(paste("Mean classification adaboost, tree = ",round(err_adab_all,2)))

res_roc = roc(ytest_adab,prob_adab)
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))


# Variable importance
plot(sort(adaboost$importance))
print("The most importante variables for prediction are")
X.desc[which(adaboost$importance>3)]

# Gradient Boosting =====================================================================
library(gbm) # bug???

# One first example
# sampling for train, test sets
samples = train_test_split(X,y,test_size=0.25)
X_train=samples$X_train
X_test=samples$X_test
y_train=samples$y_train
y_test=samples$y_test
ytest_adab = c(ytest_adab,y_test)
# standardization
mk=apply(X_train,2,mean)
sk=apply(X_train,2,sd)
X_train = scale(X_train,center=mk,scale=sk)
X_test = scale(X_test,center=mk,scale=sk)
data_train = data.frame(y_train,X_train)
data_test = data.frame(y_test,X_test)
colnames(data_train)[1] = "y"
colnames(data_test)=colnames(data_train)
data_train$y = as.numeric(data_train$y)
data_train$y = data_train$y-1
# learning decision tree mode
gb = gbm(y~.,data=data_train, shrinkage=0.01, distribution = 'bernoulli', cv.folds=5, n.trees=500, verbose=F)
best.iter = gbm.perf(gb,method="cv")

B = 30 # number of samples for the cross validation

err_gbm = NULL
prob_gbm = NULL
ytest_gbm = NULL
for (b in 1:B){
  print(paste("b =",b))
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_gbm = c(ytest_gbm,y_test)
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  data_train = data.frame(y_train,X_train)
  data_test = data.frame(y_test,X_test)
  colnames(data_train)[1] = "y"
  colnames(data_test)=colnames(data_train)
  data_train$y = as.numeric(data_train$y)
  data_train$y = data_train$y-1
  # learning decision tree mode
  gb = gbm(y~.,data=data_train, shrinkage=0.01, distribution = 'bernoulli', n.trees=500, verbose=F)
  prob_gbm = c(prob_gbm,predict(gb,data_test,n.trees=500,type="response"))
}  

res_roc = roc(ytest_gbm,prob_gbm)
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))



# eXtreme Gradient Boosting =====================================================================
library(xgboost)

ygb = as.numeric(y)-1

B = 30 # number of samples for the cross validation

err_xgb = NULL
prob_xgb = NULL
ytest_xgb = NULL
for (b in 1:B){
  # sampling for train, test sets
  samples = train_test_split(X,ygb,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_xgb = c(ytest_xgb,y_test)
  # standardization
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  dtrain <- xgb.DMatrix(as.matrix(X_train), label = y_train)
  dtest <- xgb.DMatrix(as.matrix(X_test), label = y_test)
  
  param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2,alpha=.5,
                objective = "reg:logistic")
  bst <- xgb.train(param, dtrain, nrounds = 30)
  # maxdepth = maximal depth of each individual tree
  # minsplit = minimum number of observations in a node to allow a split
  y_pred= predict(bst,dtest) 
  prob_xgb = c(prob_xgb,y_pred)
  
}

res_roc = roc(ytest_xgb,prob_xgb)
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))
