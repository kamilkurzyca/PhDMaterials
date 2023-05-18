library(MASS)
library(rpart)
cr<-Cars93 


set.set(289)

Cars93['typ3']
typ<-ifelse(cr$Type=="Large"|cr$Type=='Van','D','SR')
typ2<-ifelse(cr$Type=="Small","M",typ)
typ3<-ifelse(cr$Type=="Sporty","SP",typ2)
cr=data.frame(cr,typ=typ3)
train_total=c(1,10)
test_total=c(1,10)

for(i in 1:10){
  b=sample(1:nrow(cr),73,replace = FALSE)
  Train=cr[b,]
  Test=cr[-b,]
  train_total[i]<-list(Train)
  test_total[i]<-list(Test)
}
train_total[2]

cars.tree<-rpart(typ~ Length+Weight+EngineSize+Horsepower+RPM,data=cr,cp=0.000001,minsplit=5)

for(i in 1:10000){
  for(j in 1:10){
    cars.tree<-rpart(typ~Length+Weight+EngineSize+Horsepower+RPM,data=train_total[j],cp=i*0.001,minsplit=5)
    mn<-predict(cars.tree,test_total[j])
  }
  leng
}
