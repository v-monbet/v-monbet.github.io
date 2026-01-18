library(FactoMineR)
library(glmnet)



url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
url = "~/Dropbox/ENSEIGNEMENT/RADO/Biscuits.csv"
biscuits=read.csv(url,sep=";")
# Extraction de la colonne fat
fat=biscuits[,1]
# Extraction des variables explicatives
X=biscuits[,-1]
# Trace d'un spectre

plot(as.numeric(X[2,]),typ="l")
title("Un exemple de spectre")
# TIn the following plot, color is varying according to the fat percent
fatn=fat/max(fat)
colors= rainbow(length(fat)*2)[rev(1:length(fat))]
i.col = rank(fat)
dev.new()
plot(as.numeric(X[1,]),col=colors[i.col[1]],typ="l",ylab = "Absorbances")
for (i in 2:length(fat)){
  lines(as.numeric(X[i,]),col=colors[i.col[i]],typ="l",ylab = "Absorbances")
}
title("Spectres NIR")

#=============================================================================
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
#=============================================================================

# Find the waves length with highest  correlation with fat percent =============
p = dim(X)[2]
rho = NULL
for (j in 1:p){
  rho[j] = cor(X[,j],fat)
}

rho_28 = sort(rho)[p-28]
keep = which(rho>=rho_28)

# Overfitting =================================================================

# sampling for train, test sets
samples = train_test_split(X[,keep],fat,test_size=4,random_state=1)
X_train=samples$X_train
X_test=samples$X_test
y_train=samples$y_train
y_test=samples$y_test



# Fit a linear model ==========================================================




# Fit Ridge and Lasso regression ==============================================
alpha_values = c(1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1,5) # valeurs lambda

