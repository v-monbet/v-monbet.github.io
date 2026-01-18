# ===================================================
#      Prediction of a categorical variable
# ===================================================
library(glmnet)
library(pROC) # ROC curve


# Cancer Relaps dataset
X.all = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct",skip=2,header=TRUE,sep = "\t")
label = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls",skip=2,header=FALSE,sep = " ")


#chemin = "~/Documents/ENSEIGNEMENT/MACHINE LEARNING/Master BMC/PROJETS/Ex_datasets/Ex2/"
#X.all = read.table(paste(chemin,"Ex2_breast_cancer_ER_train_set_for_other_methods.gct",sep=""),skip=2,header=TRUE,sep = "\t")
#label = read.table(paste(chemin,"Ex2_breast_cancer_ER_train_set_class_labels.cls",sep=""),skip=2,header=FALSE,sep = " ")

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




# Fit Ridge and Lasso regression ==============================================
n = dim(X)[1] 
alpha_values = c(1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1,5,10) # valeurs lambda


B = 30 # number of samples for the cross validation
err_ridge = err_lasso = matrix(0,B,length(alpha_values))
ytest = NULL
prob_ridge = prob_lasso = NULL
for (b in 1:B){
  samples = train_test_split(X,y,test_size=.25)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test  
  ytest=c(ytest,y_test)
  # find the "best" genes
  SNR = genes_SNR(X_train,y_train)
  keep = sort.int(SNR,index.return=TRUE,decreasing=TRUE)$ix[1:50]
  # standardization
  mk=apply(X_train,2,mean)
  sk=apply(X_train,2,sd)
  X_train = scale(X_train,center=mk,scale=sk)
  X_test = scale(X_test,center=mk,scale=sk)
  prob_tmp_ridge = prob_tmp_lasso = NULL
  for (i in 1:length(alpha_values)){
    ridge = glmnet(X_train[,keep],y_train,alpha=0,lambda=alpha_values[i],family="binomial")
    y_pred = predict(ridge,as.matrix(X_test[,keep]),typ="class")
    err_ridge[b,i] = mean(y_pred!=y_test)
    prob_tmp_ridge = cbind(prob_tmp_ridge,predict(ridge,as.matrix(X_test[,keep]),typ="respons"))
    lasso = glmnet(X_train[,keep],y_train,alpha=1,lambda=alpha_values[i],family="binomial")
    y_pred = predict(lasso,as.matrix(X_test[,keep]),typ="class")
    err_lasso[b,i] = mean(y_pred!=y_test)
    prob_tmp_lasso = cbind(prob_tmp_lasso,predict(lasso,as.matrix(X_test[,keep]),typ="respons"))
    
  }
  prob_ridge = rbind(prob_ridge,prob_tmp_ridge)
  prob_lasso = rbind(prob_lasso,prob_tmp_lasso)
}

err_ridge_all = apply(err_ridge,2,mean) # global error
err_lasso_all = apply(err_lasso,2,mean) # global error
par(mfrow=c(1,2)) # Two subfigures
ylim=range(err_ridge_all,err_lasso_all)
plot(log(alpha_values),err_ridge_all,pch=20,ylim=ylim,
     xlab="log(alpha)",ylab="errors") # error for each value of k
points(log(alpha_values),err_lasso_all,pch=17,col="red",ylim=ylim) # error for each value of k
grid()

k0 = which.min(err_ridge_all) # to find for which values of k the error is min
res_roc_ridge = roc(ytest,prob_ridge[,k0])
k0 = which.min(err_lasso_all) # to find for which values of k the error is min
res_roc_lasso = roc(ytest,prob_lasso[,k0])
plot(res_roc_ridge) # ROC curve
text(.5,.2,paste("AUC ridge =",round(res_roc_ridge$auc,2)))
lines(res_roc_lasso,col="red") # ROC curve
text(.5,.1,paste("AUC lasso =",round(res_roc_lasso$auc,2)))


