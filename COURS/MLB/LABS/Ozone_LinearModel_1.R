

# Data importation
# On lit le tableau comme un tableau de chaînes de caractères,
# on extrait les colonnes numériques, 
# on fabrique artisanalement les dummies avec les colonnes catégorielles,
# on reconstitue le tout.
url="https://perso.univ-rennes1.fr/bernard.delyon/tp/ozone.txt"
Ozone = read.table(url,header=TRUE)

y=Ozone[,1]
X=Ozone[,-1]


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

B = 30 # number of samples for the cross validation
mse_lr = mse_naive = matrix(0,B,1)
for (b in 1:B){
  # sampling for train, test sets
  samples = train_test_split(X,y,test_size=0.25,random_state=1)
  X_train=samples$X_train
  X_test=samples$X_test
  y_train=samples$y_train
  y_test=samples$y_test

  # standardization
  numvar = 1:10 # list of quantitative variables
  mk=apply(X_train[,numvar],2,mean)
  sk=apply(X_train[,numvar],2,sd)
  X_train[,numvar] = scale(X_train[,numvar],center=mk,scale=sk)
  X_test[,numvar] = scale(X_test[,numvar],center=mk,scale=sk)
  data_train = data.frame(X_train,y_train)
  data_test = data.frame(X_test,y_test)
  
  # learning linear model
  lr = lm(y_train~.,data=data_train)
  y_pred = predict(lr,data_test) # prediction for the test set
  mse_lr[b] = mean((y_test-y_pred)^2) # compute MSE

  y_pred_naive = mean(y_train)
  mse_naive = mean((y_test-y_pred_naive)^2)
}

print(paste("Error of linear regression: ", round(sqrt(mean(mse_lr))*100)/100))
print(paste("Error of naive prediction: ", round(sqrt(mean(mse_naive))*100)/100))


