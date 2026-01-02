require(foreign)
ceb <- read.dta("http://data.princeton.edu/wws509/datasets/ceb.dta") 
ceb

eff = ceb$n
eff[68:72] = c(0,ceb$n[68:70],0)

eff = ceb$mean
eff[68:72] = c(0,ceb$mean[68:70],0)


ceb$y <- round(ceb$mean*ceb$n, 0)
ceb$os = log(ceb$n)  
m0 <- glm( y ~ offset(os), data=ceb, family=poisson)
summary(m0)

tapply(ceb$y, ceb$res, mean)
     Suva     Urban     Rural 
 66.70833 101.52174 289.82609 

m1r <- glm( y ~ res + offset(os), data=ceb, family=poisson)
summary(m1r)

plot(m1r$fitted.values~ceb$res)
abline(a=0,b=1)

plot(m1r,which=1,pch=20)


 
 
 
 
dur.mean = c(2,7,12,17,22,27)
par(mfrow = c(2,2) )
matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(1,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(5,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(9,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,8,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("None")
matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(2,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(6,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(10,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,8,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("Lower primary")
matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(3,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(7,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(11,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,8,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("Upper primary")

matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(4,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(8,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(12,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,8,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("Secondary and +")
#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/ceb.png",width=600,height=500)

eff = ceb$var
eff[68:72] = c(0,ceb$mean[68:70],0)


dur.mean = c(2,7,12,17,22,27)
par(mfrow = c(2,2) )
matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(1,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(5,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(9,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,12,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("None")
matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(2,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(6,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(10,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,12,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("Lower primary")
matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(3,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(7,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(11,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,12,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("Upper primary")

matplot(t(matrix(c(dur.mean,dur.mean),6,2)),t(matrix(c(rep(0,6),c(eff)[seq(4,72,by=12)]),6,2)),typ='l',lwd = 3,col="black",lty=1,ylim=range(eff),xlab="Marital duration",ylab="Mean number of children")
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.3,t(matrix(c(rep(0,6),c(eff)[seq(8,72,by=12)]),6,2)),typ='l',lwd = 3,col="blue",lty=1,add=TRUE)
matplot(t(matrix(c(dur.mean,dur.mean),6,2))+.6,t(matrix(c(rep(0,6),c(eff)[seq(12,72,by=12)]),6,2)),typ='l',lwd = 3,col="magenta",lty=1,add=TRUE)
legend(2,12,c("Suva","Urban","Rural"),col=c("black","blue","magenta"),lty=1,lwd=3)
title("Secondary and +")

#dev.print(png,"~/Dropbox/ENSEIGNEMENT/GLM_pharma/Figures/ceb_var.png",width=600,height=500)
