summarize <-
function(resu,redfile,Th=5){
# Function to summarize a specific result called by summary_mQTL
# Input:  2D results of LOD scores
# Output: Summary

    for(i in 1:length(resu)){assign(names(resu)[i],resu[[i]])}
 
    overT<-unique(which(resu$res>=Th,arr.ind=TRUE)[,2])
    if(length(overT)==0){
       overT<-unique(which(resu$res>=max(resu$res)*4/5,arr.ind=TRUE)[,2])
    }
    summar<-data.frame(check.names=FALSE)
    min_max<-read.table(redfile,header=TRUE)
    for(i in overT){
      sc<-resu$best
      sc[,"lod"]<-resu$res[,i]
      if(length(resu$permo)>0){
        summar<-rbind(summar,cbind(min_max[i,1],min_max[i,2],resu$ppm[i],summary(sc, perms=resu$permo[[i]], alpha=0.1, pvalues=TRUE)))
      }else{
        summar<-rbind(summar,(cbind(min_max[i,1],min_max[i,2],resu$ppm[i],summary(sc,Th))))
      }
    }
    colnames(summar)[1:3]<-c("Min","Max","ppm")
    return(summar)
}
