# Digits clustering
rm(list=ls())

if (!"FactoMineR" %in% installed.packages()) install.packages("FactoMineR")
if (!"smacof" %in% installed.packages()) install.packages("smacof")
if (!"vegan" %in% installed.packages()) install.packages("vegan")
if (!"Rtsne" %in% installed.packages()) install.packages("Rtsne")
if (!"fields"%in% installed.packages()) install.packages("fields")

#---------------------------------------------------------------------
# Data download
url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/digits_extrait_images.csv"
digits = read.csv(url,header=TRUE,sep=",")
digits = digits[,-1]

url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/digits_extrait_labels.csv"
labels = read.csv(url,header=TRUE,sep=",")
labels = labels[,2]


#---------------------------------------------------------------------
# PCA
library(FactoMineR)

nc = 20 # You can try to change the number of components
pca = PCA(digits,nc=nc,graph=FALSE) 

barplot(pca$eig[,1])
title("Eigen values")

barplot(pca$eig[1:50,1])
title("Eigen values (zoom)")

# Plot of the individuals with a color by label



# Reconstruction
pca_reconstruction = function(pca){
  Xr = pca$svd$U%*%diag(pca$svd$vs[1:dim(pca$svd$U)[2]])%*%t(pca$svd$V)
  Xr = Xr*t(matrix(pca$call$ecart.type,dim(pca$svd$V)[1],dim(pca$svd$U)[1]))+t(matrix(pca$call$centre,dim(pca$svd$V)[1],dim(pca$svd$U)[1]))
  return(Xr)
}
Xr = pca_reconstruction(pca)


img_list = c(0,3,1,7,15)+1
dev.new()
par(mfrow=c(2,5))
for (j in img_list){
  image(matrix(unlist(digits[j,]),28,28)[,rev(1:28)])
}
for (j in img_list){
  image(matrix(Xr[j,],28,28)[,rev(1:28)])
}

# MDS =========================================================================


# t-SNE =======================================================================

