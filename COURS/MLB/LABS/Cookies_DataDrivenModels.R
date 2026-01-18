url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
url = "~/Dropbox/ENSEIGNEMENT/RADO/Biscuits.csv"
biscuits=read.csv(url,sep=";")
# Extraction de la colonne fat
fat=biscuits[,1]
# Extraction des variables explicatives
X=biscuits[,-1]


# knn ==============================================
library(caret)

n = dim(X)[1] 
nb_neighbors = c(1,2,5,10)

B = 30 # number of samples for the cross validation
mse_knn = matrix(0,B,length(nb_neighbors))
for (b in 1:B){
  itrain = sample(1:n,round(n*9/10))
  itest = setdiff(1:n,itrain)
  Xs_train = scale(X[itrain,])
  y_train = fat[itrain]
  TRAIN = Xs_train
  Xs_test = scale(X[itest,],center=apply(X[itrain,],2,mean),scale=apply(X[itrain,],2,sd))
  TEST = Xs_test
  colnames(TEST) = colnames(TRAIN)
  for (i in 1:length(nb_neighbors)) {
    knn_res = knnreg(x=TRAIN[,-1],y=TRAIN[,1],k=nb_neighbors[i]) 
    y_pred = predict(knn_res,TEST[,-1])
    mse_knn[b,i] = mean((y_pred-fat[itest])^2)
  }
}

print(paste("Error of knn method:", round(sqrt(min(apply(mse_knn,2,mean)))*100)/100))

plot(nb_neighbors,sqrt(apply(mse_knn,2,mean)),pch=20)

# tree ==============================================
library(rpart)

n = dim(X)[1] 

B = 30 # number of samples for the cross validation
mse_tree = matrix(0,B,1)
for (b in 1:B){
  itrain = sample(1:n,round(n*9/10))
  itest = setdiff(1:n,itrain)
  Xs_train = scale(X[itrain,])
  y_train = fat[itrain]
  TRAIN = data.frame(cbind(y_train,Xs_train))
  colnames(TRAIN)[1] = "fat"
  Xs_test = scale(X[itest,],center=apply(X[itrain,],2,mean),scale=apply(X[itrain,],2,sd))
  TEST = data.frame(cbind(fat[itest],Xs_test))
  colnames(TEST) = colnames(TRAIN)
  tree = rpart(fat~.,data=TRAIN,control = rpart.control(minsplit=4,cp=0.0001))
  y_pred = predict(tree,TEST)
  mse_tree[b] = mean((y_pred-fat[itest])^2)
}

print(paste("Error of the regression tree:", round(sqrt(min(apply(mse_tree,2,mean))),3))

