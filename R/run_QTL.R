run_QTL <-
function(genofile,data,group,pgm,ppm,wd,nm,st=5,err=0.01,nperms=0){

  np=length(ppm)
  res = matrix(nrow=nm,ncol=np)
  k=1:np
  cross=read.cross(genfile=genofile,phefile=cbind(t(data[k,group]),pgm), format="gary",pnamesfile=c(paste(ppm[k],rep("ppm",np)),"pgm"),dir=wd)
  cross=calc.genoprob(cross, step=st, error.prob=err)
  ehk = matrix(nrow=nm,ncol=np)
  for (i in 1:np){
    #lip.male.ehk[,i]<-scanone(lip.male.cross,pheno.col=i,method ="ehk",chr=c('-X'))[,"lod"]
    best<-scanone(cross,pheno.col=i,method ="ehk",addcovar=pgm,maxit=100000)
    ehk[,i]<-best[,"lod"]
  }
  res[,k]=ehk
  top<-ceiling(which.max(res)/nm)
  best[,"lod"]<-ehk[,top]
  maxi<-rownames(best)[which.max(res)-(top-1)*nm]
  maxiL<-round(max(res),2)
  print(paste("SRV Sex corrected ehk in this area was",maxiL,"for",ppm[top],"ppm at",maxi))
  if (nperms>0){
    permo<-scanone(cross,pheno.col=top,method ="ehk",addcovar=pgm,n.perm=nperms)
  }else{
    permo=NULL
  }
  summary(best,maxiL-1)
  return( list(res=res,best=best,top=top,maxi=maxi,maxiL=maxiL,permo=permo))
}
