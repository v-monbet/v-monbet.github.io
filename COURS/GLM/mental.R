library(MASS)
library(VGAM)

MH = read.table(file="~/Dropbox/ENSEIGNEMENT/GLM_pharma/DATASETS/mental.txt",header=TRUE)

plot(mental~as.factor(ses),data=MH)
plot(mental~as.factor(life),data=MH)


# Les variables sont ordonnees, on peut regarder si on obtient des resultats exploitable en les considerant comme des donnees quantitatives

pca = PCA(MH)
# Le cercle des correlations indique que life sera un meilleur predicteur que ses pour mental.
# Le graphe des individus nous quels individus se ressemblent 

# 1. Modèle linéaire
pairs(~mental+ses+life, data=MH,lower.panel=panel.smooth,pch=20)

lm.mh = lm(mental ~ life+ses, data = MH)
summary(lm.mh)
# le modèle linéaire confirme ce qu'on a observé sur l'acp : la variable life (consideree comme une variable continue) apporte le plus d'information. 


# 2. Modèle multinomial - variable ses seule
lapply(MH[, c("mental", "ses", "life")], table)
Fses = ftable(xtabs(~ ses+mental , data = MH)) # on construit un tableau de contingence

MH$mental = factor(MH$mental,ordered = is.ordered(1:4))
MHses = matrix(c(0,1,as.matrix(Fses)),2,5) 
colnames(MHses) = c("ses","m1","m2","m3","m4")
MHses = as.data.frame(MHses)
MHses
plot(0:1,log(MHses[,2]/apply(MHses[,3:5],1,sum)),col="red",xlab="SES",ylab="Logit",typ="l",ylim=c(-1.5,2),axes=FALSE,lty=2,lwd=2)
box()
axis(1,at=0:1) ; axis(2,at=seq(-1.5,2,by=.5)) ; grid()
lines(0:1,log(apply(MHses[,2:3],1,sum)/apply(MHses[,4:5],1,sum)),col="magenta",lty=2,lwd=2)
lines(0:1,log(apply(MHses[,2:4],1,sum)/MHses[,5]),col="blue",lty=2,lwd=2)
# Les pentes ne sont pas égales, mais il est possible que les différences observees ne soient pas significatives. On va commencer par construire un modele multinomial a pentes egales.  

#fit1 = vglm(mental~ses,data=MH,propodds) # proba non cumulées
fit = vglm(cbind(m1,m2,m3,m4)~ses,data=MHses,family = cumulative(parallel=TRUE)) # proba cumulées
summary(fit)
abline(a=coef(fit)[1],b=coef(fit)[4],col="red")
abline(a=coef(fit)[2],b=coef(fit)[4],col="magenta")
abline(a=coef(fit)[3],b=coef(fit)[4],col="blue")
# Une autre façon d'ajuster le meme modele dont l'appel est plus simple :
fit.ses = vglm(as.ordered(mental)~ses,data=MH,family = cumulative(parallel=TRUE)) # proba cumulées
# On peut tester la qualite d'ajustement
# H_0 :  Le modele est bon | H_1 : La variable ses ne suffit pas a expliquer mental
deviance(fit.ses)
df.residual(fit.ses)
1-pchisq(deviance(fit.ses),df=df.residual(fit.ses)) # On conclut que le modèle est bon. L'hypothèse de pentes paralleles semble raisonnable. 


# On ajuste quand meme le modele a pentes non paralleles pour voir. 
fit.np = vglm(cbind(m1,m2,m3,m4)~ses,data=MHses,family = cumulative(parallel=FALSE)) # proba cumulées
summary(fit.np) # Le modele est sature et on reproduit exactement les pentes observees. 
abline(a=coef(fit.np)[1],b=coef(fit.np)[4],col="red",lty=1)
abline(a=coef(fit.np)[2],b=coef(fit.np)[5],col="magenta",lty=1)
abline(a=coef(fit.np)[3],b=coef(fit.np)[6],col="blue",lty=1)



# 2. Modèle multinomial - variable life seule

Flife = ftable(xtabs(~ life+mental , data = MH)) # on construit un tableau de contingence
MHlife = matrix(c(0:9,as.matrix(Flife)),10,5) 
colnames(MHlife) = c("life","m1","m2","m3","m4")
Mlife = as.data.frame(MHlife)
MHlife
plot(0:9,log(MHlife[,2]/apply(MHlife[,3:5],1,sum)),col="red",xlab="life",ylab="Logit",typ="l",ylim=c(-1.5,1.5),axes=FALSE,lty=2,lwd=2)
box()
axis(1,at=0:9) ; axis(2,at=seq(-1.5,2,by=.5)) ; grid()
lines(0:9,log(apply(MHlife[,2:3],1,sum)/apply(MHlife[,4:5],1,sum)),col="magenta",lty=2,lwd=2)
lines(0:9,log(apply(MHlife[,2:4],1,sum)/MHlife[,5]),col="blue",lty=2,lwd=2)
# La variation des logits en fonction de la variable life n'est pas lineaire.
# Peut-etre serait-il utile de considerer la variable life comme une variable categorielle

fit.life = vglm(as.ordered(mental)~life,data=MH,family = cumulative(parallel=TRUE)) # proba cumulées
deviance(fit.life)
df.residual(fit.life)
# On peut tester la qualite d'ajustement
# H_0 :  Le modele est bon | H_1 : La variable ses ne suffit pas a expliquer mental
1-pchisq(deviance(fit.life),df=df.residual(fit.life)) # On conclut que le modèle est bon. L'hypothèse de pentes paralleles semble neanmoins raisonnable. 
abline(a=coef(fit.life)[1],b=coef(fit.life)[4],col="red")
abline(a=coef(fit.life)[2],b=coef(fit.life)[4],col="magenta")
abline(a=coef(fit.life)[3],b=coef(fit.life)[4],col="blue")


fit2 = vglm(as.ordered(mental)~ses+life,data=MH,family = cumulative(parallel=TRUE)) # proba cumulées
deviance(fit2)
df.residual(fit2)

# Les deux appels ci-dessous sont proches. Dans le premier on ajuste un modele pour les probabilites comulees P(mental<=j) et dans le second P(mental>=j). Consequence :  les signes des parametres sont inverses. 
fit = vglm(as.ordered(mental)~life+ses,data=MH,family = cumulative(parallel=TRUE))
summary(fit)
# Interpretation des parametres. On interprete les odds-ratio. Une augementation d'un point de l'indice life reduit de 30% (=1-exp(0.31)) l'odds ratio (P(mental<=j)/(1-P(mental<=j))). Autrement dit, plus le patient se sent bien dans sa vie moins le risque que son etat mental se degrade est important. 
# Une augmentation d'un point de ses dégrade l'etat mental fortement exp(1.1) = 3.0. 
fit1 = vglm(as.ordered(mental)~ses+life,data=MH,family = propodds)
summary(fit1)
# On peut aussi utiliser la fonction polr (qui n'ajuste que des modeles multinomiaux a pentes parralleles)
fit2 = polr(mental~ses+life,data=MH)
# Pour cette fonction, on peut construire la table d'analyse des estimations des beta
(ctable <- coef(summary(fit2)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

