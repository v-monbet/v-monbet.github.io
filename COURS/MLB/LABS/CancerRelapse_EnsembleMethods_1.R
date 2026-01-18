# ===================================================
#      Prediction of a categorical variable
# ===================================================
library(pROC) # ROC curve
library(randomForest) # random forest
library(adabag) # bagging


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


# ==============================================================================
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

# Random Forest =====================================================================


B = 30 # number of samples for the cross validation
n_est_list=c(20,50,100,500)
err_rf = matrix(0,B,length(n_est_list))
prob_rf = NULL
ytest_rf = NULL
for (b in 1:B){
  print(paste("b =",b))
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_rf = c(ytest_rf,y_test)
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  
  data_train = data.frame(y_train,X_train)
  data_test = data.frame(y_test,X_test)
  colnames(data_train)[1] = "y"
  colnames(data_test) =  colnames(data_train)
  
  # learning knn model with various values of the number of neighbors
  prob_tmp = NULL
  for (k in 1:length(n_est_list)){
    rf = randomForest(y~.,data=data_train,ntree=n_est_list[k],mtry=20, control=rpart.control(maxdepth=5, minsplit=15),probability=TRUE)
    y_pred = predict(rf,data_test)
    err_rf[b,k] = mean((y_test!=y_pred)) # compute MSE
    y_prob = predict(rf,data_test,type="prob")[,2]
    prob_tmp = cbind(prob_tmp,y_prob)
  }
  prob_rf = rbind(prob_rf,prob_tmp)
}  
err_rf_all = apply(err_rf,2,mean) # global error
par(mfrow=c(1,2)) # Two subfigures
plot(n_est_list,err_rf_all,pch=20) # error for each value of k
grid()

k0 = which.min(err_rf_all) # to find for which values of k the error is min
res_roc = roc(ytest_rf,prob_rf[,k0])
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))



# Variable importance (last fitted forest)
plot(rf$importance)
print("The most importante variables for prediction are")
which(rf$importance>1)


var_imp =  sort(rf$importance,decreasing=TRUE, 
                     index.return=TRUE)
best_var_imp=var_imp$x[1:20]
best_var_names =  attributes(rf$importance)$dimnames[[1]][var_imp$ix[1:20]]



# ==============================================================================
# Bagging algorithm for classification
#### Computation time is LONG... ####
B = 30 # number of samples for the cross validation
n_est_list=c(20,50,100,500)
err_bag = matrix(0,B,length(n_est_list))
prob_bag = NULL
ytest_bag = NULL
for (b in 1:B){
  print(paste("b=",b))
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test
  ytest_bag = c(ytest_bag,y_test)
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  
  data_train = data.frame(y_train,X_train)
  data_test = data.frame(y_test,X_test)
  colnames(data_train)[1] = "y"
  colnames(data_test) =  colnames(data_train)
  
  # learning knn model with various values of the number of neighbors
  prob_tmp = NULL
  for (k in 1:length(n_est_list)){
    bag = bagging(y~.,data=data_train,mfinal=n_est_list[k],control=rpart.control(maxdepth=5, minsplit=15),par=TRUE)
    y_pred = predict(bag,data_test)
    err_bag[b,k] = mean((y_test!=y_pred$class)) # compute MSE
    prob_tmp = cbind(prob_tmp,y_pred$prob[,2])
  }
  prob_bag = rbind(prob_bag,prob_tmp)
}  
err_bag_all = apply(err_bag,2,mean) # global error
par(mfrow=c(1,2)) # Two subfigures
plot(n_est_list,err_bag_all,pch=20) # error for each value of k
grid()

k0 = which.min(err_bag_all) # to find for which values of k the error is min
res_roc = roc(ytest_bag,prob_bag[,k0])
plot(res_roc) # ROC curve
text(.5,.2,paste("AUC =",round(res_roc$auc,2)))

