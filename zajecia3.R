brach<-brach3.5klas
library(MASS)
library(stats)
library(class)
for (i in 1:length(brach$LOC)) {
    brach.lda=lda(LOC ~.,data=brach[-c(i), ])
    brach.pred=predict(brach.lda,newdata=brach)
    if (i==1) predykcja=brach.pred$class[1]
    else
    predykcja=c(predykcja,brach.pred$class[i])
}
print(tabl<-table(brach$LOC,predykcja))
print(procent<-100.0*sum(diag(tabl))/sum(tabl))



PredPodzbior<-function(num){
  brach.lda=lda(LOC ~.,data=brach[-c(num), ])
  brach.pred=predict(brach.lda,newdata=brach)
  return(pred=brach.pred$class[num])
}


numery<-sample(c(1:length(brach$LOC)),replace=FALSE)
predykcja=PredPodzbior(num=numery[1:18])
for (j in 1:9){
  predykcja=c(predykcja,PredPodzbior(num=numery[((19+(j-1)*15):(18+j*15))]))
}
print(tabl<-table(brach$LOC[numery],predykcja))
print(procent<-100.0*sum(diag(tabl))/sum(tabl))

num.ucz<-c(1:103)
num.test<-c(104:153)

set.seed(9812)
proc<-rep(0,100)
for(j in 1:100){
  numery<-sample(c(1:length(brach$LOC)),replace=FALSE)
  brach.lda=lda(LOC ~.,data=brach[-c(numery[num.ucz]), ])
  brach.pred=predict(brach.lda,newdata=brach)$class[c(numery[num.test])]
  tabl<-table(brach$LOC[c(numery[num.test])], brach.pred)
  proc[j]<-100.0*sum(diag(tabl))/sum(tabl)
  
}
  
print(tabl)
print(mean(proc)) 




u3.glm=glm(presence ~., data=urine,family=binomial)
post1<-u3.glm$fitted.values
progi<-sort(c(0,post1,1))
m=length(urine$presence[urine$presence=='no'])
n=length(urine$presence[urine$presence=='yes'])
TPF<-rep(0,length(progi))
FPF<-rep(0,length(progi))

for (i in 1:length(progi)){
  pom1<-ifelse(post1>progi[i],1,0)
  TPF[i]=length(c(1:(m+n))[pom1==1 & urine$presence=='yes'])/n
  FPF[i]=length(c(1:(m+n))[pom1==1 & urine$presence=='no'])/m
}
ROC.x<-FPF
ROC.y<-TPF
plot(ROC.x,ROC.y,type='l')


m<-40
n<-40
set.seed(2728)
X<-rnorm(m,0,1)
Y<-rnorm(n,1.5,1)
proba<-c(X,Y)  
klasa<-c(rep(0,m),rep(1,m))

progi<-sort(c(proba,min(proba)-0.5,max(proba)+0.5))
TPF<-rep(0,length(progi))
FPF<-rep(0,length(progi))
for (i in 1:length(progi)){

  pom1<-ifelse(proba>progi[i],1,0)
  TPF[i]=length(c(1:(m+n))[pom1==1 & klasa==1])/n
  FPF[i]=length(c(1:(m+n))[pom1==1 & klasa==0])/m
}            
plot(FPF,TPF,type="l")
points(FPF,TPF,pch=20)



ert=glm(formula=y ~ body + surface, data=equake,family=binomial)
plot(ert$fitted.values)
Ypred<-ifelse(ert$fitted.values<0.5,0,1)

equake<-data.frame(y=ifelse(earthquake$popn=='equake',0,1),body=earthquake$body,surface=earthquake$surface)
equake<-equake[sample(nrow(equake)),]
print(ert$fitted.values-equake$y)
plot(ert$fitted.values)


n<-length(equake$y)
predykcja<-rep(-1,n)
for (i in 1:n){
  g2=glm(y~ body + surface, data=equake[-c(i), ], family=binomial)
  Z<-predict(g2,equake)
  Yp<-exp(Z[i])/(1+exp(Z[i]))
  predykcja[i]<-ifelse(Yp>0.5,2,2)
  
  
}

print(kl<-table(equake$y,predykcja))
procent<-sum(diag(kl))/sum(kl)
print(procent)




set.seed(99) # required to reproduce the results
rnum<- sample(rep(1:150),replace=TRUE)

iris<- iris[rnum,] #randomize "iris" dataset

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new<- as.data.frame(lapply(iris[,c(1,2,3,4)],normalize))
iris.train<- iris.new[1:130,]
iris.train.target<- iris[1:130,5]
iris.test<- iris.new[131:150,]
iris.test.target<- iris[131:150,5]
model1<- knn(train=iris.train, test=iris.test, cl=iris.train.target, k=16)
table(iris.test.target, model1)

for (k in 1:100){

rnum<- sample(rep(1:150),replace=TRUE)

iris<- iris[rnum,]
iris.train<- iris[1:130,]
iris.train.target<- iris[1:130,5]
iris.test<- iris[131:150,]
iris.test.target<- iris[131:150,5]
model1<- knn(train=iris.train, test=iris.test, cl=iris.train.target, k=16)
predictions<-predict.train(object=model_knn,iris.test[,1:4], type="raw")


}
