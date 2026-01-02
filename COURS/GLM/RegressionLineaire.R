fpe <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat")
fpe
cn = colnames(fpe)
colnames(fpe)= c("Niv. social","Effort","Dec. tx nat.") # pour le graphique 1
pairs(fpe[,c("Niv. social","Effort","Dec. tx nat.")],pch=20)
dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig1.png",width=500,height=400)

#colnames(fpe) <- cn
attach(fpe)

# Régression linéaire simple ========================
colnames(fpe)= c("NivSocial","Effort","DecTxNat") # pour le graphique 1
m1<- lm(DecTxNat ~ NivSocial,data=fpe)
summary(m1)
anova(m1)
plot(setting,change,xlim=c(35,100),pch=20)
abline(coef(m1))
adj <- data.frame( pos=rep(4,nrow(fpe)), jit=0, row.names=row.names(fpe))
adj[c("CostaRica","TrinidadTobago"),"pos"] <- 2
adj[c("CostaRica","TrinidadTobago"),"jit"] <- c(1,-1)
text(setting, change+adj$jit, row.names(fpe), pos=adj$pos, cex=0.75)
dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/fig2.png",width=600,height=480)

# Régression linéaire multiple ======================
m2 <- lm(DecTxNat ~ NivSocial+Effort,data=fpe)
summary(m2)

# Analyse de la variance à un facteur ==========
attach(fpe)
NivSocial.g <- cut(NivSocial, 
   breaks=c(min(NivSocial),70,80,max(NivSocial)),
   right=FALSE,include.lowest=TRUE, 
   labels=c("Low","Medium","High"))
data.frame(min = tapply(NivSocial, NivSocial.g, min),
            max = tapply(NivSocial, NivSocial.g, max))
tapply(DecTxNat, NivSocial.g, mean)
m1g <- lm(DecTxNat ~ NivSocial.g)
 summary(m1g)
 anova(m1g)




