simple.plot <-
function(file="ur.alig.dat",lo=4.00,hi=4.10,k=1:168,title="Peak calling consensus")
{
  # Plot NMR profile plus SRV regions 


  d1<-read.csv(file)
  f<-dim(d1)[[2]]
  dat<-d1[,-c(f-2,f-1,f)]
  ppm<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(dat)))))
  st<-which(ppm>=lo & ppm<=hi)
  start=min(st)
  size=length(st)
  NMR.plot(dat,ppm,start,size,k=k,title=title)

  #mtext("Overall Profile")
}
