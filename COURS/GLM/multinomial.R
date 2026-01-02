library(locfit)
library(nnet)

data(chemdiab)
head(chemdiab)
summary(chemdiab)

attach(chemdiab)
chemdiab$cc <- ordered(chemdiab$cc, levels = c("Normal", "Chemical_Diabetic", "Overt_Diabetic"))

# Modèle de regression multinomial, lien logit
mod.m = multinom(cc~.,data=chemdiab,family=multinomial)
summary(mod.m)

z <- summary(mod.m)$coefficients/summary(mod.m)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2

z.test1 = t(rbind(z[1,],p[1,]))
colnames(z.test1) <- c("z","p_value")
z.test1

z.test2 = t(rbind(z[2,],p[2,]))
colnames(z.test2) <- c("z","p_value")
z.test1


step(mod.m)

# Modèles emboités
models <- list(
  null = multinom(cc~1,data=chemdiab,family=multinomial),
  ga = multinom(cc~ga,data=chemdiab,family=multinomial),
  ga.fpg = multinom(cc~ga+fpg,data=chemdiab,family=multinomial,maxit = 500),
  ga.fpg.ina = multinom(cc~ga+fpg+ina,data=chemdiab,family=multinomial,maxit = 500),
  ga.fpg.ina.rw = multinom(cc~ga+fpg+ina+rw,data=chemdiab,family=multinomial,maxit = 500),
  ga.fpg.ina.rw.sspg = multinom(cc~ga+fpg+ina+rw+sspg,data=chemdiab,family=multinomial,maxit = 500)
)
dev = matrix(c(deviance(models$null),deviance(models$ga),
   deviance(models$ga.fpg),deviance(models$ga.fpg.ina),deviance(models$ga.fpg.ina.rw),deviance(models$ga.fpg.ina.rw.sspg)),6,1)
-diff(dev)
qchisq(.95,1)

# Sélection lasso
mod.lasso = glmnet(as.matrix(chemdiab[,-dim(chemdiab)[2]]),chemdiab$cc,family="multinomial")
plot(mod.lasso)

cv =cv.glmnet(as.matrix(chemdiab[,-dim(chemdiab)[2]]),chemdiab$cc,family="multinomial")
mod.lasso = glmnet(as.matrix(chemdiab[,-dim(chemdiab)[2]]),chemdiab$cc,family="multinomial",lambda=cv$lambda.1se)



