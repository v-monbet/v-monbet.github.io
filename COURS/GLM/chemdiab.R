### -----------------------------------------------------------------------------
### Données chemdiab
#install.packages("locfit")
library(locfit)

data(chemdiab)
head(chemdiab)
summary(chemdiab)

attach(chemdiab)
chemdiab$cc <- ordered(chemdiab$cc, levels = c("Normal", "Chemical_Diabetic", "Overt_Diabetic"))

tt = quantile(chemdiab$ga,probs=seq(0,1,length.out=10))
p.no = matrix(0,1,length(tt))
p.cd =  matrix(0,1,length(tt))
p.od =  matrix(0,1,length(tt))
l.no = sum(chemdiab=="Normal")
l.od = sum(chemdiab=="Overt_Diabetic")
l.cd = sum(chemdiab=="Chemical_Diabetic")
for (k in 2:length(tt)){
	l.w0 = sum(chemdiab$ga>tt[k-1] & chemdiab$ga<=tt[k])
	w = which(chemdiab$ga>tt[k-1] & chemdiab$ga<=tt[k] & chemdiab$cc=="Normal")
	if (length(w)>0) {p.no[k] = length(w)/l.w0}
	w = which(chemdiab$ga>tt[k-1] & chemdiab$ga<=tt[k] & chemdiab$cc=="Chemical_Diabetic")
	if (length(w)>0) {p.cd[k] = length(w)/l.w0}
	w = which(chemdiab$ga>tt[k-1] & chemdiab$ga<=tt[k] & chemdiab$cc=="Overt_Diabetic")
	if (length(w)>0) {p.od[k] = length(w)/l.w0}
}
plot(tt,p.no,col="blue",pch=18,ylim=c(0,1),xlab="Glycémie test",ylab="Proportion de chaque classe")
lines(tt,p.no,col="blue",lty=3)
points(tt,p.cd,col="orange",pch=17)
lines(tt,p.cd,col="orange",lty=3)
points(tt,p.od,col="red",pch=19)
lines(tt,p.od,col="red",lty=3)
legend(1000,.8,legend=c("Normal", "Chemical Diabetic", "Overt Diabetic"),col=c("blue","orange","red"),lty=c(1,1,1))


library(nnet)
mod.m = multinom(cc~ga,data=chemdiab,family=multinomial)
tt.new = seq(min(chemdiab$ga),max(chemdiab$ga),length.out=100)
p = predict(mod.m,data.frame(ga=tt.new),type="probs")
lines(tt.new,p[,1],col="blue",typ="l",lwd =1.5)
lines(tt.new,p[,2],col="orange",typ="l",lwd =1.5)
lines(tt.new,p[,3],col="red",typ="l",lwd =1.5)
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig28.png",width=500,height=400)


plot(chemdiab$ga,(chemdiab$cc),pch=20,xlab="Test glucose",ylab="Group",axes=FALSE)
box()
axis(2,at=1:3,labels=levels(chemdiab$cc))
axis(1)
dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig21.png",width=500,height=400)

mod.lm = lm(as.numeric(cc)~ga)
abline(a=mod.lm$coefficients[1],b=mod.lm$coefficients[2],col="grey")
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig22.png",width=500,height=400)


# Variable binaire -> on recode la variable cc
w = which(chemdiab$cc!="Normal")
chemdiab$cc[w] = "Diabetic"
chemdiab$cc <- ordered(chemdiab$cc, levels = c("Normal", "Diabetic"))
attach(chemdiab)

plot(chemdiab$ga,(chemdiab$cc),pch=20,xlab="Test glucose",ylab="Group",axes=FALSE)
box()
axis(2,at=1:2,labels=levels(chemdiab$cc))
axis(1)

plot(chemdiab$ga,(chemdiab$cc),pch=20,xlab="Test glucose",ylab="Group",axes=FALSE)
box()
axis(2,at=1:2,labels=levels(chemdiab$cc))
axis(1)
tt = quantile(chemdiab$ga,probs=seq(0,1,length.out=20))
p.cc = NULL
for (k in 2:length(tt)){
	w = which(chemdiab$ga>tt[k-1] & chemdiab$ga<=tt[k])
	p.cc[k] = mean(as.numeric(chemdiab$cc[w]))
}
points(tt,p.cc,col="red",pch=17)
lines(tt,p.cc,col="red",lwd =1.5)
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig23.png",width=500,height=400)


logistic.fit = glm(cc~ga,family = binomial)
summary(logistic.fit)

logistic.fit = glm(cc~fpg,family = binomial)
summary(logistic.fit)
(SRP = sum(residuals(logistic.fit, type = "pearson")^2))


Jobj = function(b,X,Y){
	Jobj = sum((Y-exp(b[1]+b[2]*X)/(1+exp(b[1]+b[2]*X)))^2)
	return(Jobj)
}
res = optim(glm1$coefficients,Jobj,X=tt[2:20],Y=p.cc[2:20]-1)
b = res$par
tt.pl = seq(min(tt),max(tt),length.out=500)
plot(chemdiab$ga,(chemdiab$cc),pch=20,xlab="Test glucose",ylab="Group",axes=FALSE,xlim=c(min(chemdiab$ga),800))
box()
axis(2,at=1:2,labels=levels(chemdiab$cc))
axis(1)
points(tt,p.cc,col="red",pch=17)
lines(tt,p.cc,col="red",lwd =1.5)
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig24.png",width=500,height=400)

lines(tt.pl,exp(b[1]+b[2]*tt.pl)/(1+exp(b[1]+b[2]*tt.pl))+1,col="blue",lwd=1.5)
dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig25.png",width=500,height=400)

mod1 = glm(cc~ga,family=binomial,data=chemdiab)
summary(mod1)

L.normal = mod1$coefficients[1]+mod1$coefficients[2]*mean(chemdiab$ga[chemdiab$cc=="Normal"]) # partie linéaire
exp( L.normal)/(1+exp( L.normal))
L.diabet = mod1$coefficients[1]+mod1$coefficients[2]*mean(chemdiab$ga[chemdiab$cc!="Normal"]) # partie linéaire
exp( L.diabet)/(1+exp( L.diabet))

tt = seq(min(ga),max(ga),length.out=500)
plot(chemdiab$ga,chemdiab$cc!="Normal",pch=20,xlab="Test glucose",ylab="Group",axes=FALSE)
box()
axis(2,at=0:1,labels=levels(chemdiab$cc))
axis(1)
L = mod1$coefficients[1]+mod1$coefficients[2]*tt
lines(tt,exp(L)/(1+exp(L)),lwd = 1.5,col="red")
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig26.png",width=500,height=400)


### Déviance pour les modèles emboités
logistic.fit1 = glm(cc~fpg,family = binomial,data=chemdiab)
deviance(logistic.fit1)
aic(logistic.fit1)

logistic.fit2 = glm(cc~fpg+rw,family = binomial,data=chemdiab)
deviance(logistic.fit2)
aic(logistic.fit2)

1-pchisq( deviance(logistic.fit1)- deviance(logistic.fit2),df=1)
