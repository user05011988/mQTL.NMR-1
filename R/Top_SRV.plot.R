Top_SRV.plot <-
function(file1="ur.alig.dat",file2="rectangle_SRV.PPM",results=results,met=met,intMeth="mean",clustidx=clustidx)
{
  # Plot NMR profile plus SRV regions 

   d1<-read.csv(file1)
   d2<-read.csv(file2, header=TRUE, sep='\t')
  
if (missing(clustidx))
{
  summa <- summarize(results, file2, 0)
  max_idx<-which.max(summa[,6])
  marker_idx<-which(summa[,4]==summa[max_idx,4] & summa[,5]==summa[max_idx,5])

  M_SRV<- summa[marker_idx,1:3]

  clustidx<-which(d2[,3]==summa[max_idx,3])
}

  f<-dim(d1)[[2]]
  fclust<-dim(d2)[[1]]
  dat<-d1[,-c(f-2,f-1,f)]
  dat2<-apply(dat,2,intMeth)
  ppmOld<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(dat)))))
  
  if (clustidx==1)
  {
   st<-which(ppmOld>=d2[clustidx,1] & ppmOld<=d2[clustidx+2,2])
   sti<-which(ppmOld>=d2[clustidx,1] & ppmOld<=d2[clustidx,2])
   st1R<-which(ppmOld>=d2[clustidx+1,1] & ppmOld<=d2[clustidx+1,2])
   st2R<-which(ppmOld>=d2[clustidx+2,1] & ppmOld<=d2[clustidx+2,2])
   SRVr<- d2[(clustidx):(clustidx+2),]  
  }else if(clustidx==2){
   st<-which(ppmOld>=d2[clustidx-1,1] & ppmOld<=d2[clustidx+2,2])
   sti<-which(ppmOld>=d2[clustidx,1] & ppmOld<=d2[clustidx,2])
   st1R<-which(ppmOld>=d2[clustidx+1,1] & ppmOld<=d2[clustidx+1,2])
   st2R<-which(ppmOld>=d2[clustidx+2,1] & ppmOld<=d2[clustidx+2,2])
   st1L<-which(ppmOld>=d2[clustidx-1,1] & ppmOld<=d2[clustidx-1,2])
   SRVr<- d2[(clustidx-1):(clustidx+2),] 
  }else if(clustidx==fclust){
   st<-which(ppmOld>=d2[clustidx-2,1] & ppmOld<=d2[clustidx,2])
   sti<-which(ppmOld>=d2[clustidx,1] & ppmOld<=d2[clustidx,2])
   st2L<-which(ppmOld>=d2[clustidx-2,1] & ppmOld<=d2[clustidx-2,2])
   st1L<-which(ppmOld>=d2[clustidx-1,1] & ppmOld<=d2[clustidx-1,2])
   SRVr<- d2[(clustidx-2):(clustidx),] 
  }else if(clustidx==fclust-1){
   st<-which(ppmOld>=d2[clustidx-2,1] & ppmOld<=d2[clustidx+1,2])
   sti<-which(ppmOld>=d2[clustidx,1] & ppmOld<=d2[clustidx,2])
   st1R<-which(ppmOld>=d2[clustidx+1,1] & ppmOld<=d2[clustidx+1,2])
   st2L<-which(ppmOld>=d2[clustidx-2,1] & ppmOld<=d2[clustidx-2,2])
   st1L<-which(ppmOld>=d2[clustidx-1,1] & ppmOld<=d2[clustidx-1,2])
   SRVr<- d2[(clustidx-2):(clustidx+1),] 
  } else {
  st<-which(ppmOld>=d2[clustidx-2,1] & ppmOld<=d2[clustidx+2,2])
  sti<-which(ppmOld>=d2[clustidx,1] & ppmOld<=d2[clustidx,2])
  st1R<-which(ppmOld>=d2[clustidx+1,1] & ppmOld<=d2[clustidx+1,2])
  st2R<-which(ppmOld>=d2[clustidx+2,1] & ppmOld<=d2[clustidx+2,2])
  st2L<-which(ppmOld>=d2[clustidx-2,1] & ppmOld<=d2[clustidx-2,2])
  st1L<-which(ppmOld>=d2[clustidx-1,1] & ppmOld<=d2[clustidx-1,2])
  SRVr<- d2[(clustidx-2):(clustidx+2),] 
  }

  start=min(st)
  end=max(st)
  
  plot(ppmOld[start:end],dat2[start:end],type="l",col="gray80",lwd=2,xlim=c(ppmOld[end],ppmOld[start]), ylim=range(dat2[start:end]),xlab="resonance, ppm",ylab="Intensity",main=paste("Top cluster at ppm",d2[clustidx,3],"+/- 2 clusters"))
  
if(exists("sti")){
  starti=min(sti)
  endi=max(sti)
  lines(ppmOld[starti:endi],dat2[starti:endi],col="red",lwd=2)
}

if(exists("st1R")){
  startR1=min(st1R)
  endR1=max(st1R)
  lines(ppmOld[startR1:endR1],dat2[startR1:endR1],col="blue",lwd=2)
}
if(exists("st2R")){
  startR2=min(st2R)
  endR2=max(st2R)
  lines(ppmOld[startR2:endR2],dat2[startR2:endR2],col="blue",lwd=2)
}
if(exists("st1L")){
  startL1=min(st1L)
  endL1=max(st1L)
  lines(ppmOld[startL1:endL1],dat2[startL1:endL1],col="blue",lwd=2)
}
if(exists("st2L")){
  startL2=min(st2L) 
  endL2=max(st2L)
  lines(ppmOld[startL2:endL2],dat2[startL2:endL2],col="blue",lwd=2)
  }

  abline(v=SRVr[,1],col=3); 
  abline(v=SRVr[,2],col=2) ;
}
