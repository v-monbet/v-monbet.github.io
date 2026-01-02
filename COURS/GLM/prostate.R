prostate = read.table(file="~/Dropbox/ENSEIGNEMENT/GLM_pharma/DATASETS/prostatedatr.txt",header=TRUE)
head(prostate)
summary(prostate)


library(FactoMineR)
acp = PCA(prostate,graph=FALSE)
par(mfrow=c(1,2))
plot(acp, choix = "var")
plot(acp, choix = "ind")
w = which(prostate$lymph==1)
points(acp$ind$coord[w,1:2],col="red",pch=20)

# On veut predire la 6eme variable (lymph). Il es tplus coherent de faire l'ACp sans cette variable pour visualiser quelle information les autres variables apportent pour lymph. 
prostate$lymph = as.factor(prostate$lymph)
acp = PCA(prostate[,-6],graph=FALSE,ncp=5) 
par(mfrow=c(1,2))
plot(acp, choix = "var")
plot(acp, choix = "ind",label ="none" )
w = which(prostate$lymph==1)
points(acp$ind$coord[w,1:2],col="red",pch=20)
# Le cercle des correlation nous permet d'interpreter les axes : le premier axe factoriel (abscisse) représente la taille et la gravité de la tumeur. Plus les individus qui sont à droite du graphique sontles individus ui ont une tumeur plus grosse et plus grave que la moyenne. Inversement les individus qui sont a gauche ont une tumeur moins grosse et moins grave que l'individu moyen. 
# L'axe 2 représente l'age et l'acide. Les indidivdus qui sont en aux du graphique sont plus ages que ceux qui sont en bas. 
# On observe que les cas (lymph=1) sont plutôt situées à droite du graphique. On s'attend alors a ce que les variables taille et gravite soient significatives avec un parametre beta positif. L'age et acid ne semblent pas etre determinants. 

par(mfrow=c(2,2))
plot(acp, choix = "var")
plot(acp$ind$coord[,1:2],pch=20,label ="none")
w = which(prostate$lymph==1)
points(acp$ind$coord[w,1:2],col="red",pch=20)
plot(acp, choix = "var",axes = c(3, 4))
plot(acp$ind$coord[,3:4],pch=20,label ="none")
points(acp$ind$coord[w,3:4],col="red",pch=20)


prostate$radio = as.factor(prostate$radio)
prostate$taille = as.factor(prostate$taille)
prostate$gravite = as.factor(prostate$gravite)
prostate$lymph = as.factor(prostate$lymph)

attach(prostate)
boxplot(age~lymph) # confirme que l'age n'est pas significatif

# recherche d'interactions entre l'age et la variables qualitatives
q.age = quantile(prostate$age,probs=seq(0,1,by=.25)) # 4 classes d'age
ncl = length(q.age)-1

age.m  <- matrix(0,1,ncl)
lymph.p0 <- lymph.p1 <- matrix(0,1,ncl)
for (k in 1:ncl){
	w = which(prostate$age>= q.age[k] & prostate$age< q.age[k+1])
	age.m[k] = mean(prostate$age[w])
    lymph.p0[k] = sum((prostate$lymph[w]==1)&(prostate$taille[w]==0))/length(w)
    lymph.p1[k] = sum((prostate$lymph[w]==1)&(prostate$taille[w]==1))/length(w)
} 
plot(age.m,lymph.p0,pch="0",ylim=range(lymph.p0,lymph.p1),main="age*taille",ylab="P(lymph=1)")
lines(age.m,lymph.p0)
points(age.m,lymph.p1,pch="1",col="red")
lines(age.m,lymph.p1,col="red")
# On observe que la probabilité d'être malade est plus forte quand la taille est plus grande, mais les probabilités ne varient pas beaucoup avec l'age


age.m  <- matrix(0,1,ncl)
lymph.p0 <- lymph.p1 <- matrix(0,1,ncl)
for (k in 1:ncl){
	w = which(prostate$age>= q.age[k] & prostate$age< q.age[k+1])
	age.m[k] = mean(prostate$age[w])
    lymph.p0[k] = sum((prostate$lymph[w]==1)&(prostate$gravite[w]==0))/length(w)
    lymph.p1[k] = sum((prostate$lymph[w]==1)&(prostate$gravite[w]==1))/length(w)
} 
plot(age.m,lymph.p0,pch="0",ylim=range(lymph.p0,lymph.p1),main="age*gravite",ylab="P(lymph=1)")
lines(age.m,lymph.p0)
points(age.m,lymph.p1,pch="1",col="red")
lines(age.m,lymph.p1,col="red")




# Pour tracer les logit, 
library(boot) # la fonction logit est définie dans le package boot
logistic <- function(x){exp(x)/(1+exp(x))} 
logit <- function(y){log(y/(1-y))} 

plot(age.m,logit(lymph.p0),pch="0",ylim=range(logit(lymph.p0),logit(lymph.p1)),ylab="logit")
lines(age.m,logit(lymph.p0))
points(age.m,logit(lymph.p1),pch="1",col="red")
lines(age.m,logit(lymph.p1),col="red")



# Modélisation (sans discrétisation)
models.0 <- list(
   null     = glm(lymph ~ 1, family=binomial, data=prostate),
   additif     = glm(lymph ~ ., family=binomial, data=prostate)
 )
summary(models.0$null)
summary(models.0$additif)# les variables taille et gravite sont assez fortement correlees (voir fleches proches dans l'ACP) : le modèle garderait la variable taille.
anova(models.0$additif,test="Chisq")
# Les donnees sont des donnees individuelles : le test base sur la deviance donne une idee de la qualite d'ajustement mais il n'est pas valide (la statistique de test ne suit pas un chi2 pour des donnees individuelles)
# On peut alors faire un test d'Hosmer-Lemeshow... 
hosmerlem = function(y, yhat, g=10) {
  cutyhat = cut(yhat,
     breaks = quantile(yhat, probs=seq(0,
       1, 1/g)), include.lowest=TRUE)
  ynum = as.numeric(y)-1
  obs = xtabs(cbind(1 - ynum, ynum) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}
# On teste H_0 : le modele permet de reproduite les donnees | H_1 le modele ne reproduit pas bien les donnees
hl = hosmerlem(prostate$lymph,fitted(models.0$additif),g=6) # on conclut que le modele est bon


# Si on compare 2 modeles, on fait un test de rapport de vraisemblance; Dans ce cas, on peut bien faire un test basee sur les deviances. 
# On teste H_0 : les deux modèles sont équivalents | H_1 au moins une des variables ajoutées dans le modèle 2 apporte de l'information pour séparer les malades des non malades
anova(models.0$null,models.0$additif,test="Chisq") # on conclut que le modèle additif est meilleur que le modèle null

m.all = glm(lymph~.^2,data=prostate,family=binomial)
m.st = step(m.all,direction="backward")
summary(m.st)
# graphes des résidus
plot(m.st)
# performances en prédiction, validation croisée 5-fold
library(pROC)
n = nrow(prostate)
set.seed("123")
ii = sample(n,n)
 Kfold = 10
nk = floor(n/Kfold)
p.fit = NULL
Y.test = NULL
m.st = list()
for (k in 1:Kfold){
   testi = ii[((k-1)*nk+1):(k*nk)]
   appri = setdiff(ii,testi)
   m.all = glm(lymph~.^2,data=prostate,family=binomial,subset=appri)
   m.st[[k]] = step(m.all,direction="backward",trace=0)
   pr = predict(m.st[[k]],prostate[testi,],type="response")
   Y.test = c(Y.test,prostate$lymp[testi])
   p.fit = c(p.fit,pr)
}
   roc.res = roc(Y.test,p.fit,plot=TRUE)
  legend(.4,.1,legend=paste("AUC =",floor(roc.res$auc*100)/100))
   str(roc.res)
   table(Y.test,p.fit>.5)

# Bootstrap
B = 100
   nk = floor(n/10)
   p.fit <- p.fit.all <- NULL
   Y.test = NULL
   for (b in 1:B){
   testi = sample(1:n,nk)
   appri = setdiff(ii,appri)
   m.all = glm(lymph~.^2,data=prostate,family=binomial,subset=appri)
   m.st = step(m.all,direction="backward",trace=0)
   pr = predict(m.st,prostate[testi,],type="response")
   Y.test = c(Y.test,prostate$lymp[testi])
   p.fit = c(p.fit,pr)
   }
   roc.res = roc(Y.test,p.fit,plot=TRUE)
  legend(.4,.1,legend=paste("AUC =",floor(roc.res$auc*100)/100))

plot(roc.res$thresholds,roc.res$sensitivities,typ="l")
lines(roc.res$thresholds,roc.res$specificities,col="blue")


library(glmnet)
tmp = cv.glmnet(cbind(prostate1[,c(1:4,6:)],xfactors),prostate1[,5],family="binomial")


models <- list(
   null     = glm(lymph ~ 1, family=binomial, data=prostate),
   radio     = glm(lymph ~ radio, family=binomial, data=prostate),
   taille     = glm(lymph ~ taille, family=binomial, data=prostate),
   add    = glm(lymph ~ radio+taille, family=binomial, data=prostate),
   mult     = glm(lymph ~ radio*taille, family=binomial, data=prostate)
 )
 res = matrix(0,length(models),2)
 colnames(res) <- c("Dev.","ddl" )
 rownames(res) <- c("null","radio","taille","add","mult")
 for (k in 1:length(models)){
 	res[k,1] = deviance(models[[k]]) 
  	res[k,2] = models[[k]]$df.residual
}
res


models <- list(
   null     = glm(lymph ~ 1, family=binomial, data=prostate),
   acid     = glm(lymph ~ acid, family=binomial, data=prostate),
   radio     = glm(lymph ~ radio, family=binomial, data=prostate),
   add    = glm(lymph ~ acid+radio, family=binomial, data=prostate),
   add.all    = glm(lymph ~ acid+radio+taille, family=binomial, data=prostate),
   mult     = glm(lymph ~ radio+taille+acid:radio, family=binomial, data=prostate),
   mult.all     = glm(lymph ~ radio+taille+acid:radio+acid:taille, family=binomial, data=prostate)
 )
 res = matrix(0,length(models),2)
 colnames(res) <- c("Dev.","ddl" )
 rownames(res) <- c("null","acid","radio","add","add.all","mult","mult.all")
 for (k in 1:length(models)){
 	res[k,1] = deviance(models[[k]]) 
  	res[k,2] = models[[k]]$df.residual
}
res

		
#===================================================
# diagnostiques

# Pearson
dr = residuals(models$add.all)
pr = residuals(models$add.all,type="pearson")
cbind(prostate[,c("acid","radio","taille")],fit = fitted(models$add.all),dr,pr)[abs(dr)>2,]
		
		# Résidus studentisés, distances de Cook, effet levier
		obs = prostate$lymp
		pfit = fitted(models$add.all)
		lev = hatvalues(models$add.all)
		i = order(-lev)
		cd = cooks.distance(models$add.all)
		cbind(prostate[,c("acid","radio","taille")],obs,pfit,lev,cd)[i[1:5],]
		
		#===================================================
		
		# validation croisée
		library(pROC)
		
		# 5 folds validation
		n = nrow(prostate)
		ii = sample(n,n)
		Kfold = 5
		nk = floor(n/Kfold)
		p.fit = NULL
		Y.test = NULL
		for (k in 1:Kfold){
			testi = ii[((k-1)*nk+1):(k*nk)]
			appri = setdiff(ii,appri)
			mod   = glm(lymph ~ acid+radio+taille, family=binomial, data=prostate,subset=appri)
			pr = predict(mod,prostate[testi,],type="response")
			Y.test = c(Y.test,prostate$lymp[testi])
			p.fit = c(p.fit,pr)
		}
		roc.res = roc(Y.test,p.fit,plot=TRUE,main=floor(roc.res$auc*100)/100)
		title(roc.res$auc)
		roc(Y.test,p.fit,plot=TRUE,smooth=TRUE,col="gray",add=TRUE)		
		
		# bootstrap validation
		B = 100
		nk = floor(n/10)
		p.fit <- p.fit.all <- NULL
		Y.test = NULL
		for (b in 1:B){
			testi = sample(1:n,nk)
			appri = setdiff(ii,appri)
			mod   = glm(lymph ~ acid+radio+taille, family=binomial, data=prostate,subset=appri)
			pr = predict(mod,prostate[testi,],type="response")
			mod.all   = glm(lymph ~ ., family=binomial, data=prostate,subset=appri)
			pr.all = predict(mod.all,prostate[testi,],type="response")
			Y.test = c(Y.test,prostate$lymp[testi])
			p.fit = c(p.fit,pr)
			p.fit.all = c(p.fit.all,pr.all)
		}
		roc.res = roc(Y.test,p.fit,plot=TRUE,main=floor(roc.res$auc*100)/100)
		roc.all = roc(Y.test,p.fit.all,plot=TRUE,col="red",add=TRUE)
		
		
		#===================================================
		# regression lasso
		library(glmnet)
		
		prostate = read.table(file="~/Dropbox/ENSEIGNEMENT/GLM_pharma/DATASETS/prostatedatr.txt",header=TRUE)
		res.cv = cv.glmnet(as.matrix(prostate[,1:5]),prostate[,6],family="binomial")
		res = glmnet(as.matrix(prostate[,1:5]),prostate[,6],family="binomial",lambda=res.cv$lambda.min)
		
		
		
# Comparaison modele additif, modele additif avec selestion backward et modele additif+Lasso		
n = nrow(prostate)
set.seed("123")
Kfold = 10
for (j in 1:5){prostate[,j]=numeric(prostate[,j])}
ii = sample(n,n)
nk = floor(n/Kfold)
p.add <- p.step <- p.lasso <- NULL
Y.test = NULL
for (k in 1:Kfold){
   testi = ii[((k-1)*nk+1):(k*nk)]
   appri = setdiff(ii,testi)
   m.add = glm(lymph~.,data=prostate,family=binomial,subset=appri)
   pr = predict(m.add,prostate[testi,],type="response")
   p.add = c(p.add,pr)
   m.st = step(m.add,direction="backward",trace=0)
   pr = predict(m.st,prostate[testi,],type="response")
   p.step = c(p.step,pr)
   tmp = cv.glmnet(as.matrix(prostate[appri,1:5]), prostate[appri,6],family="binomial")
   m.lasso = glmnet(as.matrix(prostate[appri,1:5]), prostate[appri,6],lambda=tmp$lambda.min)
   pr = predict(m.lasso,as.matrix(prostate[testi,1:5]))
   p.lasso = c(p.lasso,pr)
   Y.test = c(Y.test,prostate$lymp[testi])
}
   roc.add = roc(Y.test,p.add,plot=TRUE)
   roc.step = roc(Y.test,p.step,plot=TRUE,add=TRUE,col="magenta")
   roc.lasso = roc(Y.test,p.lasso,plot=TRUE,add=TRUE,col="red")
legend(.4,.1,legend=paste("AUC =",floor(roc.res$auc*100)/100))
		
		