arrow.plot <-
function(file="ur.alig.dat",lo=4.00,hi=4.10,k=1:168,title="Peak calling consensus")
{
  # Plot NMR profile plus SRV regions and consensus across the various statistics


  d1<-read.csv(file)
  f<-dim(d1)[[2]]
  dat<-d1[,-c(f-2,f-1,f)]
  ppm<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(dat)))))
  st<-which(ppm>=lo & ppm<=hi)
  start=min(st)
  size=length(st)
  mm<-max(dat[start:(start+size)])
  NMR.plot(dat,ppm,start,size,k=k,title=title,ylim=c(-mm/3,mm))
  par(new=TRUE)
  n=start:(start+size)*3

  consensus<-read.table("consensus.dat")
  me<-read.table("mean_SRV.ppm",header=TRUE)
  med<-read.table("median_SRV.ppm",header=TRUE)
  ma<-read.table("max_SRV.ppm",header=TRUE)
  su<-read.table("sum_SRV.ppm",header=TRUE)
  tr<-read.table("trapeze_SRV.ppm",header=TRUE)
  re<-read.table("rectangle_SRV.ppm",header=TRUE)
  plot(consensus[n,2],-consensus[n,3],ty="b",pch=3,yaxt='n',ylim=c(-70,210),ylab="",xlab="",axes=FALSE,cex=0.5);
  axis(4,labels=c(64,32,16,8,4,2,1,0),at=c(-64,-32,-16,-8,-4,-2,-1,0))
  mtext("Consensus Score",4)
  arrows(me[,1],rep(-55,dim(me)[[1]]),me[,2],rep(-55,dim(me)[[1]]),col="red",pch=4,length=0.1,angle=15);
  arrows(ma[,1],rep(-50,dim(ma)[[1]]),ma[,2],rep(-50,dim(ma)[[1]]),col="green",pch=3,length=0.1,angle=15);
  arrows(su[,1],rep(-45,dim(su)[[1]]),su[,2],rep(-45,dim(su)[[1]]),col="blue",pch=2,length=0.1,angle=15);
  arrows(med[,1],rep(-40,dim(med)[[1]]),med[,2],rep(-40,dim(med)[[1]]),col=6,pch=6,length=0.1,angle=15);
  arrows(tr[,1],rep(-35,dim(tr)[[1]]),tr[,2],rep(-35,dim(tr)[[1]]),col=7,pch=7,length=0.1,angle=15);
  arrows(re[,1],rep(-30,dim(re)[[1]]),re[,2],rep(-30,dim(re)[[1]]),col=8,length=0.1,angle=15);
}
