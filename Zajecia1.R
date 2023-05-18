
install.packages("ape")
library(ape)
library(MASS)

m.mean<-apply(Miasta,2,mean)
m.std<-sqrt(apply(Miasta,2,var))
miasta<-Miasta
mistastand<-sweep(miasta,2,m.mean)
mistastand<-sweep(mistastand,2,FUN="/",m.std)



miastastand.pc<-princomp(~., cor=FALSE,data=mistastand)
print(summary(miastastand.pc))


# wartosci wlasne
print((miastastand.pc$sdev)^2)

plot(miastastand.pc)

miastastand.pc$loadings

miastastand.pc$scores



which.max(miastastand.pc$scores[,1])



biplot(miastastand.pc,choices=1:2,pc.biplot=TRUE,cex=0.6)

UScrimestand<-UScrime
print(UScrime)
rek.mean<-apply(UScrime[,-c(16)],2,mean)
rek.std<-sqrt(apply(UScrime[,-c(16)],2,var))
UScrimestand[,-c(16)]<-sweep(UScrimestand[,-c(16)],2,rek.mean)
UScrimestand[,-c(16)]<-sweep(UScrimestand[,-c(16)],2,FUN="/",rek.std)
UScrime<-UScrimestand
print(UScrime)
rmse<-function(x,y) sqrt(mean(x-y)^2)

model1=lm(y ~.,data=UScrime)
print(summary(model1))

stepAIC(model1,direction="backward")

model2<-lm(y~M+Ed+Po1+M.F+U1+U2+Ineq+Prob+GDP,data=UScrime)
