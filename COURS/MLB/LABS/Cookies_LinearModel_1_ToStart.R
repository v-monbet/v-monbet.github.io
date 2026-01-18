
url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
biscuits=read.csv(url,sep=";")
# Extraction de la colonne fat
fat=biscuits[,1]
# Extraction des variables explicatives
X=biscuits[,-1]
# Trace d'un spectre


plot(as.numeric(X[2,]),typ="l")
title("Un exemple de spectre")
# TIn the following plot, color is varying according to the fat percent
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

# Find the waves legth with highest  correlation with fat percent =============
p = dim(X)[2]
rho = NULL
for (j in 1:p){
  rho[j] = cor(X[,j],fat)
}

rho_10 = sort(rho)[p-10]
keep = which(rho>=rho_10)

# Compute PCA components ======================================================
# Standardisation
library(FactoMineR)

# Plot the eigenvalues
pca = PCA(X,nc=10) # nc = nombre de composantes
dev.new() # ouvre une fenetre graphique
barplot(pca$eig[,2]) # Ebouli des valeurs propres
title('Pourcentage of explained variance')

# Fit a linear model ==========================================================
# And use cross-validation

train_test_split <- function (X,y,test_size=.25,random_state=NULL){
  if (is.integer(test_size)){test_size=test_size/n}
  if (!is.null(random_state)){set.seed(random_state)}
  n = nrow(X)
  itest=sample(1:n,round(n*test_size))
  itrain=setdiff(1:n,itest)
  Xtrain=X[itrain,]
  Xtest=X[itest,]
  ytrain=y[itrain]
  ytest=y[itest]
  return(list(X_train=Xtrain,X_test=Xtest,y_train=ytrain,y_test=ytest))
}
