
### -----------------------------------------------------------------------------
### Données contraception

# Modèle binomial, données agrégées
cuse <- data.frame(matrix(c(
 1, 0,  58, 265,
 1, 1,  14,  60,
 2, 0,  68, 215,
 2, 1,  37,  84,
 3, 0,  79, 230,
 3, 1, 158, 145,
 4, 0,  14,  43,
 4, 1,  79,  58), 8, 4, byrow=TRUE))
names(cuse) <- c("age","nomore","using","notUsing")
cuse$n <- cuse$using + cuse$notUsing
#cuse$nomore <- factor(cuse$nomore, labels=c("nomore","more"))
cuse$age <- factor(cuse$age, labels=c("< 25","25-29","30-39","40-49"))
cuse$Y <- cbind(cuse$using, cuse$notUsing)
desire      = glm(Y ~ nomore, family=binomial, data=cuse)
summary(desire)
models <- list(
   null     = glm(Y ~ 1, family=binomial, data=cuse),
   age      = glm(Y ~ age, family=binomial, data=cuse),
   desire   = glm(Y ~ nomore, family=binomial, data=cuse),
   additive = glm(Y ~ age + nomore, family=binomial, data=cuse),
   interact = glm(Y ~ age * nomore, family=binomial, data=cuse)
 )


# Modèle à un facteur
cuse <- data.frame(matrix(c(
  0, 219, 753, 
  1, 288, 347), 2, 3, byrow=TRUE))
names(cuse) <- c("nomore","using","notUsing")
cuse$n <- cuse$using + cuse$notUsing
cuse$Y <- cbind(cuse$using, cuse$notUsing)

models <- list(
   null     = glm(Y ~ 1, family=binomial, data=cuse),
   desire      = glm(Y ~ n, family=binomial, data=cuse)
)


# Modèle à deux facteurs
cuse <- data.frame(matrix(c(
 1,  72, 325,
 2, 105, 299,
 3, 237, 375,
 4,  93, 101), 4, 3, byrow=TRUE))
names(cuse) <- c("age","using","notUsing")
cuse$n <- cuse$using + cuse$notUsing
cuse$age <- factor(cuse$age, 
      labels=c("< 25","25-29","30-39","40-49"))
cuse
cuse$Y <- cbind(cuse$using, cuse$notUsing)
mag <- glm( Y ~ age, family=binomial, data=cuse)
summary(mag)


# Analyse de la covariance
cuse$agem <- c(20, 27.5, 35, 45)[as.numeric(cuse$age)]
cuse$agec <- cuse$agem - 30.6
ancova = list(
one      = glm(Y ~ agem, family=binomial, data=cuse),
parallel = glm(Y ~ agem+nomore, family=binomial, data=cuse),
two      = glm(Y ~ agec*nomore, family=binomial, data=cuse) )
cuse$more <- 1 - cuse$nomore
cuse$age.more   <- cuse$agec * cuse$more
cuse$age.nomore <- cuse$agec * cuse$nomore
alt <- glm(Y ~ more + age.more + nomore + age.nomore - 1, family=binomial, data=cuse)

plot(cuse$agem, predict(models$interact), pch=16, 
     ylim=c(-1.5,.5), xlab="age", ylab="logit",
     main="Contraceptive Use by Age and Desire")
linear <- predict(ancova$two)
n <- cuse$nomore == 1
lines(cuse$agem[n], linear[n])
lines(cuse$agem[!n], linear[!n])

AIC = c(models$null$aic, models$age$aic,models$desire$aic,models$additive$aic,models$interact$aic)
AIC.c = c(NA,ancova$one$aic,NA,ancova$parallel$aic,ancova$two$aic)
plot(AIC,axes=FALSE,ylim=range(AIC,AIC.c,na.rm=TRUE),pch=20,xlab="model",ylab="AIC")
box()
axis(1,at=1:5,labels=c("null","age","nomore","age+nomore","age*nomore"))
points(AIC.c,pch=18,col="red")
grid()

# two parabolas, same curvature
curvy <-  glm(Y ~ agec* nomore + I(agec^2), family=binomial, data=cuse)
agesc <- ages - 30.6
lines(ages, b[1]       +  b[2]       * agesc  + b[4] * agesc^2, lty="dashed")
lines(ages,(b[1]+b[3]) + (b[2]+b[5]) * agesc  + b[4] * agesc^2, lty="dashed")

deviance(ancova$two) - deviance(curvy)


AIC = c(models$null$aic, models$age$aic,models$desire$aic,models$additive$aic,models$interact$aic,NA)
AIC.c = c(NA,ancova$one$aic,NA,ancova$parallel$aic,ancova$two$aic,NA)
plot(AIC,axes=FALSE,ylim=range(c(AIC,AIC.c,curvy$aic),na.rm=TRUE),pch=20,xlab="Modèle",ylab="AIC")
box()
axis(1,at=1:6,labels=c("null","age","nomore","age+nomore","age*nomore","quadratic"))
points(AIC.c,pch=18,col="red")
points(c(rep(NA,5),curvy$aic),col="blue",pch=17)
grid()
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig_aic.png",width=500,height=400)

BIC = c(BIC(models$null), BIC(models$age),BIC(models$desire),BIC(models$additive),BIC(models$interact),NA)
BIC.c = c(NA,BIC(ancova$one),NA,BIC(ancova$parallel),BIC(ancova$two),NA)
plot(BIC,axes=FALSE,ylim=range(c(BIC,BIC.c,BIC(curvy)),na.rm=TRUE),pch=20,xlab="Modèle",ylab="BIC")
box()
axis(1,at=1:6,labels=c("null","age","nomore","age+nomore","age*nomore","quadratic"))
points(BIC.c,pch=18,col="red")
points(c(rep(NA,5),BIC(curvy)),col="blue",pch=17)
grid()
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig_bic.png",width=500,height=400)


### -----------------------------------------------------------------------------
### Sélection, validation de modèle


df<-read.table('http://www.indiana.edu/~statmath/stat/all/cdvm/gss_cdvm.csv', sep=',', header=T)
df$www = as.factor(df$www)
df$male = as.factor(df$male)
df$trust = as.factor(df$trust)
attach(df)
summary(df[,-2])
head(df)

# Binary Logit Model
blm<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="logit"))
summary(blm)

blm$deviance/-2
AIC(blm)

LRtest<-blm$null.deviance-blm$deviance
LRtest
LRp<-dchisq(LRtest, blm$df.null - blm$df.residual)
LRp
1-blm$deviance/blm$null.deviance # McFadden's pseudo R square

# Erreur de prédiction
appri = sample(nrow(df),nrow(df)*2/3)
testi = setdiff(1:nrow(df),appri)
blm<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="logit"))
predr = predict(blm,df[testi,],type="response")
(c.r=table(predr>.5,df$trust[testi]))
(err.r = 1-sum(diag(c.r))/length(testi))
mean(abs(as.numeric(trust[testi])-1-predr)>.5)
blm<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="logit"),subset=appri)
predcv = predict(blm,df[testi,],type="response")
(c.cv = table(predcv>.5,df$trust[testi]))
(err.cv = 1-sum(diag(c.cv))/length(testi))

library(boot)
blm<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="logit"))
cout = function(trust,prev_prob){
	c = mean(abs(trust-prev_prob)>.5)
	return(c)
}
cv.glm(df,blm,cout)$delta
cv.glm(df,blm,cout,K=5)$delta


s.seq = seq(0.05,.95,by=.05)
se <- sp <- matrix(0,length(s.seq),1)
cnt = 1
for (s in s.seq){
	tab = table(predcv>s,df$trust[testi])
	if (sum(predcv<=s)>0) {se[cnt] = tab[2,2]/sum(tab[,2])
	}else {se[cnt]=1  }
	if (sum(predcv>s)>0) {sp[cnt] = tab[1,1]/sum(tab[,1])}
	else {sp[cnt]=1 }
	
	cnt = cnt+1
}
sp[1] = 0 ; sp[17:19] = 1 # a revoir... 
plot(s.seq,se,typ="l",lwd = 2,ylab=" ",xlab= "Seuil")
lines(s.seq,sp,col="red",lty=2,lwd=2)
legend(0.6,0.8,legend=c("Se","Sp"),lwd=c(2,2),lty=1:2,col=1:2)
grid()

#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/SeSp.png",width=500,height=400)

library(pROC)
r=roc(df$trust[testi],predcv)
plot(r,main=paste("AUROC =",round(r$auc*100)/100))
grid()

#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/ROC.png",width=500,height=400)

# Estimation K-fold de la courbe ROC
K = 10
nk = floor(nrow(df)/K) 
pred = NULL
Yobs = NULL
rk=list()
ii = sample(nrow(df),nrow(df)) # permutation aléatoire des indices
for (k in 1:K){
	testi = ii[((k-1)*nk+1):(k*nk)]
	appri = setdiff(1:nrow(df),testi)
	blm<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="logit"),subset=appri)
	pr = predict(blm,df[testi,],type="response")
	
	pred = c(pred,pr)
	Yobs = c(Yobs,df$trust[testi])
    rk[[k]]=roc(df$trust[testi],pr)
}
add = FALSE
for (k in 1:K){
	if (k>1) {add=TRUE}
	plot.roc(rk[[k]],col="gray",add=add,lwd=1)
}
r=roc(Yobs,pred)
plot(r,main=paste("AUROC =",round(r$auc*100)/100),add=TRUE)
grid()
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/ROC_Kfold.png",width=500,height=400)

# Comparaison de modèles
K = 10
nk = floor(nrow(df)/K) 
pred = NULL
Yobs = NULL
rk=list()
ii = sample(nrow(df),nrow(df)) # permutation aléatoire des indices
for (k in 1:K){
	testi = ii[((k-1)*nk+1):(k*nk)]
	appri = setdiff(1:nrow(df),testi)
	blm<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="logit"),subset=appri)
	blm1<-glm(trust~educate+income+age+male+www, data=df, family=binomial(link="probit"),subset=appri)
	pr = predict(blm,df[testi,],type="response")
	
	pred = c(pred,pr)
	Yobs = c(Yobs,df$trust[testi])
    rk[[k]]=roc(df$trust[testi],pr)
}
add = FALSE
for (k in 1:K){
	if (k>1) {add=TRUE}
	plot.roc(rk[[k]],col="gray",add=add,lwd=1)
}
r=roc(Yobs,pred)
plot(r,main=paste("AUROC =",round(r$auc*100)/100),add=TRUE)
grid()

