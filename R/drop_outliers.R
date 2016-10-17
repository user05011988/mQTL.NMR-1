drop_outliers <-
function(data,ppm,n){
  #par(mfrow=c(2,1))
  nppm<-length(ppm)
  for(p in 1:nppm){
  #  hist(lip.SRVlog10_1[p,],br=50,main=paste(p,": Before"))
    for (i in 1:n){
      data[p,]<-rm.outlier(data[p,] ,fill=TRUE,median=TRUE)
    }
   # hist(lip.SRVlog10_1[p,],br=50,main="After")
  }
 return(data)
}
