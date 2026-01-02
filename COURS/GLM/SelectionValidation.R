### Comparaison de modèles, bootstrap+ROC
#
library(rpart)
data(kyphosis)
summary(kyphosis)
n = nrow(kyphosis)
ntest = round(n/5)
p.add <- p.mul <- Yobs <- p.st <- NULL
B = 100
for (b in 1:B){
	testi = sample(1:n,ntest)
	appri = setdiff(1:n,testi)
	mod = glm(Kyphosis~.,data=kyphosis,family=binomial,subset=appri)
	pmod = predict(mod,kyphosis[testi,])
	mod2 = glm(Kyphosis~.^2,data=kyphosis,family=binomial,subset=appri)
	pmod2 = predict(mod2,kyphosis[testi,])
	Yobs = c(Yobs,kyphosis$Kyphosis[testi])
	p.add = c(p.add,pmod)
	p.mul = c(p.mul,pmod2)	p.st = c(p.st,pst2)
}
r.add=roc(Yobs,p.add)
plot(r.add)
r.mul=roc(Yobs,p.mul)
plot(r.mul,add=TRUE,col="blue",lty=2)
legend(.4,.4,legend=c("sans interactions","avec interactions"),lty=1:2,col=c("black","blue"),lwd=2)
legend(.4,.2,legend=c(paste("AUROC =",round(r.add$auc*100)/100),paste("AUROC =",round(r.mul$auc*100)/100)),lty=1:2,col=c("black","blue"),lwd=2)
grid()

#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/ROC_CompMod.png",width=600,height=600)

mod2 = glm(Kyphosis~.^2,data=kyphosis,family=binomial)
mod2.st = step(mod2)
summary(mod2.st)

# Méthodes de sélection backward 
df<-read.table('http://www.indiana.edu/~statmath/stat/all/cdvm/gss_cdvm.csv', sep=',', header=T)
df$www = as.factor(df$www)
df$male = as.factor(df$male)
df$trust = as.factor(df$trust)

blm = glm(trust~(educate+income+age+male+www)^2, data=df, family=binomial)
blm.st = step(blm,direction="backward")

# regression lasso
df<-read.table('http://www.indiana.edu/~statmath/stat/all/cdvm/gss_cdvm.csv', sep=',', header=T)
df1 = df[,-2]
df1$EducInc = df$educate*df$income
df1$EducAge = df$educate*df$age
df1$EducMale = df$educate*df$male
df1$EducWww = df$educate*df$www
df1$IncAge = df$income*df$age
df1$IncMale = df$income*df$male
df1$IncWww = df$income*df$www
df1$AgeMale = df$age*df$male
df1$AgeWww = df$age*df$www
df1$MaleWww = df$male*df$www

library(glmnet)
blm.lasso = glmnet(as.matrix(df1[,-1]),df1[,1],family="binomial")
plot(blm.lasso,xvar="lambda")
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/LassoTraj.png",width=500,height=400)

blm.cv = cv.glmnet(as.matrix(df1[,-1]),df1[,1],family="binomial")
blm.lasso = glmnet(as.matrix(df1[,-1]),df1[,1],family="binomial",lambda=blm.cv$lambda.min)

n = nrow(df)
ntest = round(n/5)
p.st <- p.lasso <- Yobs <- p.st <- NULL
B = 100
for (b in 1:B){
	testi = sample(1:n,ntest)
	appri = setdiff(1:n,testi)
	blm = glm(trust~(educate+income+age+male+www)^2, data=df, family=binomial,subset=appri)
	blm.st = step(blm,direction="backward")	
	pmod = predict(blm.st,df[testi,])
	blm.cv = cv.glmnet(as.matrix(df1[appri,-1]),df1[appri,1],family="binomial")
	blm.lasso = glmnet(as.matrix(df1[appri,-1]),df1[appri,1],family="binomial",lambda=blm.cv$lambda.min)
	pmod2 = predict(blm.lasso,as.matrix(df1[testi,-1]),type="response")
	Yobs = c(Yobs,df$trust[testi])
	p.st = c(p.st,pmod)
	p.lasso = c(p.lasso,pmod2)	
}
library(pROC)
r.st=roc(Yobs,p.st)
r.lasso=roc(Yobs,p.lasso)
plot(r.st)
plot(r.lasso,add=TRUE,col="blue",lty=4)
legend(.4,.4,legend=c("Backward","Lasso"),lty=c(1,4),col=c("black","blue"),lwd=2)
grid()
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/ROC_backw_lasso.png",width=600,height=600)


# Jean-Philippe Vert
# source("https://bioconductor.org/biocLite.R")
# biocLite("ALL")
# library(ALL)
# data(ALL)
# x <- t(exprs(ALL))
# y <- substr(ALL$BT,1,1)


# n = length(y)
# ntrain <- round(n * 0.8) # number of training examples
# tindex <- sample(n, ntrain) # indices of training samples
# xtrain <- x[tindex, ]
# xtest <- x[-tindex, ]
# ytrain <- y[tindex]
# ytest <- y[-tindex]





