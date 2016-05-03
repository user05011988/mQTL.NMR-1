SRV.plot <-
function(file1="ur.alig.txt",file2="rectangle_SRV.PPM",lo=4,hi=4.8,k=1,title="Cluster plot"){
 # Plot the region [lo:hi]

  d1<-read.csv(file1)
  d2<-read.csv(file2, header=TRUE, sep='\t')

  f<-dim(d1)[[2]]
  data<-d1[,-c(f-2,f-1,f)]
  ppmOld<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(data)))))
  
  st<-which(ppmOld>=lo & ppmOld<=hi)
  x=min(st)
  t=length(st)

  NMR.plot(data,ppmOld,x,t,k=k,title=title)

  clustIdx<-which(d2[,1]>=lo & d2[,2]<=hi)
  SRVr<- d2[clustIdx,] 

  abline(v=SRVr[,1],col=3); 
  abline(v=SRVr[,2],col=2) ;
}
