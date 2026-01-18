library(gbm) 
library(xgboost)




url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
#url = "~/Dropbox/ENSEIGNEMENT/RADO/Biscuits.csv"
biscuits=read.csv(url,sep=";")
# Extraction de la colonne fat
fat=biscuits[,1]
# Extraction des variables explicatives
X=biscuits[,-1]

y = fat

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


# Gradient Boosting =====================================================================





# eXtreme Gradient Boosting =====================================================================

