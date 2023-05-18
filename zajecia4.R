
iris
B=100
bootstrap=list()
booterr_pes=as.numeric()
booterr_pes_total=0
booterr_opt_total=0
set.seed(45)
for( i in 1:B){
  b=sample(1:nrow(iris),replace = TRUE)

  Train=iris[b,]
  Test=iris[-b,]
  cl  <- Train[1:150,5]
  model_boot<-knn(Train[1:nrow(Train),1:4],Test[1:nrow(Test),1:4],cl,k=1) 
  booterr_pes_total<-booterr_pes_total+sum(ifelse(model_boot!=Test[,5],1,0))/nrow(Test)
  
  model_boot_opt<-knn(Train[1:nrow(Train),1:4],iris[1:nrow(iris),1:4],cl,k=1)
  booterr_opt_total<-booterr_opt_total+sum(ifelse(model_boot!=Test[,5],1,0))/nrow(iris)
  
}

score=0.632*(booterr_opt_total/B)+0.368*(booterr_pes_total/B)
print(score)



