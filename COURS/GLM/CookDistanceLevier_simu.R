set.seed("123")
n = 20
x = rnorm(n,1)
y = x+.2*rnorm(n,1)
plot(x,y,pch=20)
m0 = lm(y~x)

w = which(x>2.5)
w0 = w[1]
x1 = x ; y1 = y
y1[w0] = y1[w0]-4
plot(x1,y1,pch=20)
m = lm(y1~x1)
par(mfrow=c(1,2))
plot(x1,y1,pch=20)
points(x1[w0],y1[w0],col="green")
abline(m)
abline(m0,lty=2)
plot(cooks.distance(m),pch=20)
points(w0,cooks.distance(m)[w0],col="green")
points(hatvalues(m),col="red",pch=17)
abline(h=2,lty=3)
which.max(cooks.distance(m))
abline(h=2*2/n,lty=3,col="red")

x3 = x ; y3 = y
y3[11] = y[11]-2
#x3[1] = x[1]+5
m = lm(y3~x3)
par(mfrow=c(1,2))
plot(x3,y3,pch=20)
points(x3[11],y3[11],col="green")
abline(m)
abline(m0,lty=2)
plot(cooks.distance(m),pch=20)
points(11,cooks.distance(m)[11],col="green")
points(hatvalues(m),col="red",pch=17)
abline(h=2,lty=3)
abline(h=2*2/n,lty=3,col="red")

x2 = x ; y2 = y
x2[20] = x[20]-2
y2[20]= y[20]-2
m = lm(y2~x2)
par(mfrow=c(1,2))
plot(x2,y2,pch=20)
abline(m)
abline(m0,lty=2)
points(x2[20],y2[20],col="green")
plot(cooks.distance(m),pch=20)
points(20,cooks.distance(m)[20],col="green")
points(hatvalues(m),col="red",pch=17)
abline(h=2,lty=3)
abline(h=2*2/n,lty=3,col="red")
