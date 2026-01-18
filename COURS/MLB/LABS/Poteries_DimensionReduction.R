
if (!"FactoMineR" %in% installed.packages()) install.packages("FactoMineR")
if (!"smacof" %in% installed.packages()) install.packages("smacof")
if (!"vegan" %in% installed.packages()) install.packages("vegan")
if (!"Rtsne" %in% installed.packages()) install.packages("Rtsne")
if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")

library(ggplot2)
#---------------------------------------------------------------------
# Data download
pot = read.table("https://perso.univ-rennes1.fr/bernard.delyon/data/poterie.dat",header=TRUE)
Four = pot$FOUR
pot = pot[,1:9]

#---------------------------------------------------------------------
# Compute PCA
library(FactoMineR)
pca = PCA(pot,ncp=2) # standardization and plots are by default



df = data.frame(cbind(pca$ind$coord,Four))
colnames(df) = c("PC1","PC2","Four")
df$Four = as.factor(df$Four)
ggplot(df, aes(x=PC1,y=PC2, color=Four)) +
  geom_point()+
  labs(title="PCA")


#---------------------------------------------------------------------
# Compute MDS
D = dist(scale(pot)) # euclidean distance
mds = cmdscale(D,k=2) 

df = data.frame(cbind(mds,Four))
colnames(df) = c("Axis_1","Axis_2","Four")
df$Four = as.factor(df$Four)
ggplot(df, aes(x=Axis_1,y=Axis_2, color=Four)) +
  geom_point() +
  labs(title="MDS")


library(smacof)
mds.smacof <- mds(D)
plot(mds.smacof, type = "p", label.conf = list(label = TRUE, col = "darkgray"), pch = 25, col = Four)


#---------------------------------------------------------------------
# Compute isomap
library(vegan)
D = dist(scale(pot)) # euclidean distance
isom = isomap(D, ndim=2,k=10)


df = data.frame(cbind(isom$points,Four))
colnames(df) = c("Axis_1","Axis_2","Four")
df$Four = as.factor(df$Four)
ggplot(df, aes(x=Axis_1,y=Axis_2, color=Four)) +
  geom_point() +
  labs(title="Isomap")

#---------------------------------------------------------------------
# Compute tSNE
library(Rtsne)

perplexity = c(1,2,5,10)

set.seed(9)  
dev.new()
par(mfrow=c(1,4))
for (perp in perplexity){
  tsne_model_1 = Rtsne(scale(pot), check_duplicates=FALSE, pca=TRUE, 
                     perplexity=perp, theta=0, dims=2)
  plot(tsne_model_1$Y,col=Four,pch=20,xlab="1st axis",ylab="2nd axis")
  title(perp)
}

