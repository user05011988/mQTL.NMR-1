process_mQTL <-
function(datfile, genfile, nperm=0){
  # Script to process the tissue extract of the individuals
  # Part of the main script
  # Input:  genotype and phenotype
  # Output: 2D LOD score table
  
  ## PROCESS THE DATA
   
  st=5
  err=0.001
  cross<-read.cross("csvs", genfile=genfile,phefile=datfile)
  cross<-jittermap(cross)
  np<-nphe(cross)-3
  ppm<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", names(cross$pheno)[1:np]))))
  k=1:np
  cross=calc.genoprob(cross, step=st, error.prob=err)
  cross=sim.geno(cross, step=st, error.prob=err,n.draws=64)
  TotM<-0
  for(i in 1:nchr(cross)){ TotM<-TotM+dim(cross$geno[[i]]$prob)[[2]] }
  print(paste("TotM is ",TotM,"and np is",np))
  res = matrix(nrow=TotM,ncol=np)
  ehk = matrix(nrow=TotM,ncol=np)
  permo = list()
  for (i in 1:np){
    best<-scanone(cross, pheno.col=i, method ="ehk")
        if (nperm>0){ 
       permo[[i]]<-scanone(cross,pheno.col=i,method ="ehk",n.perm=nperm,verbose=FALSE) 
           }
    ehk[,i]<-best[,"lod"]
    if(i%%min(round(np/10),100)==0){
      print(c(i,max(ehk,na.rm=TRUE)))
      summary(best)
    }
  }
  res[,k]=ehk
  top<-ceiling(which.max(res)/TotM)
  best[,"lod"]<-res[,top]
  maxi<-rownames(best)[which.max(res)-(top-1)*TotM]
  maxiL<-round(max(res),2)
  print(paste("SRV ehk in this area was",maxiL,"for",ppm[top],"ppm at",maxi))
  if(nperm>0){
    summary(best,perms=permo[[top]],pvalues=TRUE)
  }else{
    summary(best,maxiL-1)
  }

  return(list(res=res,ppm=ppm,best=best,top=top,maxi=maxi,maxiL=maxiL,permo=permo))
}
