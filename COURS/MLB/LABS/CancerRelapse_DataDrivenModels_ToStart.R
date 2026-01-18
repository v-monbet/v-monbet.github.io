# ===================================================
#      Prediction of a categorical variable
# ===================================================
library(class)
library(rpart)


# Cancer Relaps dataset
X.all = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct",skip=2,header=TRUE,sep = "\t")
label = read.table("https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls",skip=2,header=FALSE,sep = " ")



X.desc = X.all[,1]
X = t(X.all[,-c(1,2)])

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

# =======================================================
# ==============================================================================
# Nearest neighbors algorithm for classification


#"""
#
# Nearest neighbors, small dimension
#
#"""



# ==============================================================================
# Decision tree



#"""
#
#Tree on PCA, small dimension
#
#"""
