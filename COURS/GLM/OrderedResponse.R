library(locfit)
library(nnet)
library(MASS)
library(VGAM)
 



trauma = matrix(c(1,2,3,4,59,48,44,43,25,21,14,4,46,44,54,49,48,47,64,58,32,30,31,41),4,6)
colnames(trauma) <- c("dose","y1","y2","y3","y4","y5")
#colnames(trauma) <- c("dose","Death","VegS","MajD","MinD","GooR")
trauma = as.data.frame(trauma)
trauma

plot(1:4,log(trauma[,2]/apply(trauma[,3:6],1,sum)),pch=20,col="red",ylim=c(-1.4,1.8),xlab="Dose",ylab="Logit",axes=FALSE)
box()
axis(1,at=1:4,label=1:4)
axis(2,at=seq(-1.4,1.8,by=.8),label=seq(-1.4,1.8,by=.8))
lines(1:4,log(trauma[,2]/apply(trauma[,3:6],1,sum)),col="red",lty=3)
points(1:4,log(apply(trauma[,2:3],1,sum)/apply(trauma[,4:6],1,sum)),pch=18,col="orange")
lines(1:4,log(apply(trauma[,2:3],1,sum)/apply(trauma[,4:6],1,sum)),col="orange",lty=3)
points(1:4,log(apply(trauma[,2:4],1,sum)/apply(trauma[,5:6],1,sum)),pch=17,col="green")
lines(1:4,log(apply(trauma[,2:4],1,sum)/apply(trauma[,5:6],1,sum)),col="green",lty=3)
points(1:4,log(apply(trauma[,2:5],1,sum)/trauma[,6]),pch=16,col="blue")
lines(1:4,log(apply(trauma[,2:5],1,sum)/trauma[,6]),col="blue",lty=3)
grid()
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig52.png",width=500,height=400)



fit = vglm(cbind(y1,y2,y3,y4,y5)~dose,family = cumulative(parallel=TRUE),data=trauma)
summary(fit)
abline(a=coef(fit)[1],b=coef(fit)[5],col="red")
abline(a=coef(fit)[2],b=coef(fit)[5],col="orange")
abline(a=coef(fit)[3],b=coef(fit)[5],col="green")
abline(a=coef(fit)[4],b=coef(fit)[5],col="blue")
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig54.png",width=500,height=400)


fit2 = vglm(cbind(y1,y2,y3,y4,y5)~dose,family = cumulative,data=trauma)
summary(fit2)
pchisq(deviance(fit)-deviance(fit2), df=df.residual(fit)-df.residual(fit2),lower.tail=FALSE)

sqrt(mean(sum((fitted(fit)-trauma)^2)))
sqrt(mean(sum((fitted(fit2)-trauma)^2)))


plot(1:4,log(trauma[,2]/apply(trauma[,3:6],1,sum)),pch=20,col="red",ylim=c(-1.4,1.8),xlab="Dose",ylab="Logit",axes=FALSE)
box()
axis(1,at=1:4,label=1:4)
axis(2,at=seq(-1.4,1.8,by=.8),label=seq(-1.4,1.8,by=.8))
lines(1:4,log(trauma[,2]/apply(trauma[,3:6],1,sum)),col="red",lty=3)
points(1:4,log(apply(trauma[,2:3],1,sum)/apply(trauma[,4:6],1,sum)),pch=18,col="orange")
lines(1:4,log(apply(trauma[,2:3],1,sum)/apply(trauma[,4:6],1,sum)),col="orange",lty=3)
points(1:4,log(apply(trauma[,2:4],1,sum)/apply(trauma[,5:6],1,sum)),pch=17,col="green")
lines(1:4,log(apply(trauma[,2:4],1,sum)/apply(trauma[,5:6],1,sum)),col="green",lty=3)
points(1:4,log(apply(trauma[,2:5],1,sum)/trauma[,6]),pch=16,col="blue")
lines(1:4,log(apply(trauma[,2:5],1,sum)/trauma[,6]),col="blue",lty=3)
grid()
abline(a=coef(fit)[1],b=coef(fit)[5],col="red")
abline(a=coef(fit)[2],b=coef(fit)[5],col="orange")
abline(a=coef(fit)[3],b=coef(fit)[5],col="green")
abline(a=coef(fit)[4],b=coef(fit)[5],col="blue")
abline(a=coef(fit2)[1],b=coef(fit2)[5],col="red",lty=2,lwd=1.5)
abline(a=coef(fit2)[2],b=coef(fit2)[6],col="orange",lty=2,lwd=1.5)
abline(a=coef(fit2)[3],b=coef(fit2)[7],col="green",lty=2,lwd=1.5)
abline(a=coef(fit2)[4],b=coef(fit2)[8],col="blue",lty=2,lwd=1.5)
dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig56.png",width=500,height=400)

trauma = matrix(c(1,2,3,4,59,48,44,43,25,21,14,4,46,44,54,49,48,47,64,58,32,30,31,41),4,6)
count = c(t(trauma[,2:6]))
dose = c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
response = c(matrix(1:5,5,4))
trauma2 = data.frame(dose=dose,response=response,count=count)
y =  factor(trauma2$response)

msat = multinom(y~dose,data=trauma2,weight=count)
summary(msat)
plot(1:4,log(trauma[,3]/trauma[,2]),pch=20,col="red",ylim=c(-2.5,.5),xlab="Dose",ylab="Logit",axes=FALSE)
box()
axis(1,at=1:4,label=1:4)
axis(2,at=seq(-2.5,.5,by=.5),label=seq(-2.5,.5,by=.5))
lines(1:4,log(trauma[,3]/trauma[,2]),col="red",lty=3)
points(1:4,log(trauma[,4]/trauma[,2]),pch=18,col="orange")
lines(1:4,log(trauma[,4]/trauma[,2]),col="orange",lty=3)
points(1:4,log(trauma[,5]/trauma[,2]),pch=17,col="green")
lines(1:4,log(trauma[,5]/trauma[,2]),col="green",lty=3)
points(1:4,log(trauma[,6]/trauma[,2]),pch=16,col="blue")
lines(1:4,log(trauma[,6]/trauma[,2]),col="blue",lty=3)
grid()
abline(a=coef(msat)[1,1],b=coef(msat)[1,2],col="red")
abline(a=coef(msat)[2,1],b=coef(msat)[2,2],col="orange")
abline(a=coef(msat)[3,1],b=coef(msat)[3,2],col="green")
abline(a=coef(msat)[4,1],b=coef(msat)[4,2],col="blue")
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig55.png",width=500,height=400)


mod.clogit = polr(y~dose,data=trauma2,weight=count)
summary(mod.clogit)
(ctable <- coef(summary(mod.clogit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

mod.cprobit = polr(y~dose,data=trauma2,weight=count,method="probit")
summary(mod.cprobit)
(ctable <- coef(summary(mod.cprobit)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

plot(1:4,qnorm(trauma[,2]/apply(trauma[,2:6],1,sum)),pch=20,col="red",ylim=c(-1,1.2),xlab="Dose",ylab="Logit",axes=FALSE)
box()
axis(1,at=1:4,label=1:4)
axis(2,at=seq(-1,1.2,by=.8),label=seq(-1,1.2,by=.8))
lines(1:4,qnorm(trauma[,2]/apply(trauma[,2:6],1,sum)),col="red",lty=3)
points(1:4,qnorm(apply(trauma[,2:3],1,sum)/apply(trauma[,2:6],1,sum)),pch=18,col="orange")
lines(1:4,qnorm(apply(trauma[,2:3],1,sum)/apply(trauma[,2:6],1,sum)),col="orange",lty=3)
points(1:4,qnorm(apply(trauma[,2:4],1,sum)/apply(trauma[,2:6],1,sum)),pch=17,col="green")
lines(1:4,qnorm(apply(trauma[,2:4],1,sum)/apply(trauma[,2:6],1,sum)),col="green",lty=3)
points(1:4,qnorm(apply(trauma[,2:5],1,sum)/apply(trauma[,2:6],1,sum)),pch=16,col="blue")
lines(1:4,qnorm(apply(trauma[,2:5],1,sum)/apply(trauma[,2:6],1,sum)),col="blue",lty=3)
grid()
abline(a=mod.cprobit$zeta[1],b=-coef(mod.cprobit),col="red")
abline(a=mod.cprobit$zeta[2],b=-coef(mod.cprobit),col="orange")
abline(a=mod.cprobit$zeta[3],b=-coef(mod.cprobit),col="green")
abline(a=mod.cprobit$zeta[4],b=-coef(mod.cprobit),col="blue")

#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/fig57.png",width=500,height=400)



plot(1:4,qnorm(trauma[,2]/apply(trauma[,2:6],1,sum)),pch=20,col="red",ylim=c(-1.4,1.8),xlab="Dose",ylab="Logit",axes=FALSE)
box()
axis(1,at=1:4,label=1:4)
axis(2,at=seq(-1.4,1.8,by=.8),label=seq(-1.4,1.8,by=.8))
lines(1:4,qnorm(trauma[,2]/apply(trauma[,2:6],1,sum)),col="red",lty=3)
points(1:4,qnorm(apply(trauma[,2:3],1,sum)/apply(trauma[,2:6],1,sum)),pch=18,col="orange")
lines(1:4,qnorm(apply(trauma[,2:3],1,sum)/apply(trauma[,2:6],1,sum)),col="orange",lty=3)
points(1:4,qnorm(apply(trauma[,2:4],1,sum)/apply(trauma[,2:6],1,sum)),pch=17,col="green")
lines(1:4,qnorm(apply(trauma[,2:4],1,sum)/apply(trauma[,2:6],1,sum)),col="green",lty=3)
points(1:4,qnorm(apply(trauma[,2:5],1,sum)/trauma[,6]),pch=16,col="blue")
lines(1:4,qnorm(apply(trauma[,2:5],1,sum)/trauma[,6]),col="blue",lty=3)


# Modèle à risques adjacents
fit.adj = vglm(cbind(y1,y2,y3,y4,y5)~dose,family = acat(parallel=TRUE),data=trauma)
summary(fit.adj)

# Modèle à risques sequentiels
fit.seq = vglm(cbind(y1,y2,y3,y4,y5)~dose,family = cratio( parallel=TRUE),data=trauma)
summary(fit.seq)
