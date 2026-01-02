diabete = read.table("~/Dropbox/ENSEIGNEMENT/GLM_Pharma/DATASETS/Diabetes_NSW.dat",header=TRUE)
diabete 

age.m = seq(20,90,by=10)
par(mfrow=c(1,2))
plot(age.m,diabete$Rate[1:8],pch=20,xlab="Age",ylab="Death rate per 100000",ylim=range(diabete$Rate))
lines(age.m,diabete$Rate[1:8])
points(age.m,diabete$Rate[9:16],pch=17,col="red")
lines(age.m,diabete$Rate[9:16],col="red")
legend(20,180,legend=c("Male","Female"),pch=c(20,17),col=c("black","red"))

plot(age.m,log(diabete$Rate[1:8]),pch=20,xlab="Age",ylab="log(Death rate per 100000)",ylim=c(-3,log(max(diabete$Rate))))
lines(age.m,log(diabete$Rate[1:8]))
points(age.m,log(diabete$Rate[9:16]),pch=17,col="red")
lines(age.m,log(diabete$Rate[9:16]),col="red")
legend(20,180,legend=c("Male","Female"),pch=c(20,17),col=c("black","red"))

attach(diabete)
l_popn = log(Population)
### categorical age
Model1 <- glm(Deaths ~ Gender + Age, family = poisson(link = log), offset = l_popn)
summary(Model1)

# Si on veut changer la modalite de reference on peut utiliser la fonction C
# On choisit comme classe de referecne les hommes entre 45 et 54 ans
Model1 <- glm(Deaths ~ C(Gender,base=2) + C(Age,base=4), family = poisson(link = log), offset = l_popn)
summary(Model1)
# Interpretation des parametres ................
# Le taux moyen de deces pour la modalite de reference (les hommes entre 45 et 54 ans) est estime a exp(-9.892) = 5e-5
# Les femmes ont un taux de deces qui est exp(-0.523) = 0.593 fois celui des hommes ie que le taus de deces des femmes est 40.7% inferieur a celui des hommes, tout âge confondu. 
# L'effet d'être dans la classe des 35-44 ans multiplu le taux moyen de deces par exp(-0.997) = 0.369 ie le le taux de deces decroit d'environ 63% pour les deux sexes. 
anova(Model1,test="Chisq")
# L'anova montre que l'ajout de la variable age, le genre etant deja dans le modele, fait decroitre la deviance de pour le 3000 pour 7 ddl. L'age est tres significatif.

age.m = c(20,30,40,50,60,70,80,90)
age.m = c(age.m,age.m)
diabete$age.m = age.m
Model1bis = glm(Deaths~Gender+age.m,family=poisson,offset=l_popn,data=diabete)
summary(Model1bis)

diabete$age.m2 = age.m^2
diabete$age.m3 = age.m^3
Model2 = glm(Deaths~Gender+age.m+age.m2+age.m3,family=poisson,offset=l_popn,data=diabete)
# Comme l'effet de l'age est non lineaire (age + age^2), l'effet de l'age sur le taux de deces depend de l'age (le taux de deces augment de plus en plus vite avec l'age). 
anova(Model2,test="Chisq")
# L'introduction des termes quadratiques et cubiques ameliore le modele comme le montre la decroissance de deviance. Le genre et le terme lineaire de l'age sont tres significatifs. L'age au carre est significatif aussi : il apporte de l'information supplemantaire a l'age et le genre. Le terme cubique n'apporte pas plus d'information (pvalue = 0.11).  

# On peut regarder si une ineraction entre le genre et l'age permet d'ameliorer le modele. 
Model3 = glm(Deaths~Gender+age.m+age.m2+age.m:Gender,family=poisson,offset=l_popn,data=diabete)
anova(Model3,test="Chisq")
# Les tests montrent que l'ajout de l'interaction entre le genre et l'age est significative (les autres termes etant deja dans le modele)

Model4 = glm(Deaths~Gender+age.m+age.m2+age.m:Gender++age.m2:Gender,family=poisson,offset=l_popn,data=diabete)
anova(Model4,test="Chisq")
# L'ajout d'une interaction entre le terme quadratique et le genre est inutile  (pvalue = 0.56)

Model3bis = glm(Deaths~Gender+Age+Age:Gender,family=poisson,offset=l_popn,data=diabete)
anova(Model3bis,test="Chisq")
# Dans le cas ou l'age est une variable categorielle, l'ajout d'une interaction entre le l'age et le genre est inutile  (pvalue = 0.14). De plus cet ajout conduit a un modele sature (df = 0)

# Choix du modele
# On peut comparer les AIC
AIC(Model1)
AIC(Model1bis)
AIC(Model2)
AIC(Model3)
AIC(Model3bis)
AIC(Model4)
# Le modele 3 a l'AIC le plus faible. 










