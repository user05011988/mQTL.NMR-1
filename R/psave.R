psave <-
function(z,p,titre,ppm,res,LT)
{# Save in a file scores of z higher than LT for this permutation p

  zgt<-which(z>LT,arr.ind=TRUE)
  if (nrow(zgt)>0){
    results<-cbind(p,z[zgt],rownames(res)[zgt[,1]],ppm[zgt[,2]])
    colnames(results)<-c("Perm","MaxScore","Marker","Shift")
    write.table(results,paste(titre,"dat",sep="."),row.names=FALSE,col.names=FALSE,quote=FALSE,append=TRUE,sep="\t")
  }
}
