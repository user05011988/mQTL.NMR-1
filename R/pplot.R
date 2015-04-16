pplot <-
function(z,title,ppm,res,LT=c(5,10,15,20))
{# Plot the results with a color scale y layer over 3 in 2D

  nm=dim(res)[1]
  which(z>LT[1],arr.ind=TRUE)-> zgt4
  which(z>LT[2],arr.ind=TRUE)-> zgt5
  which(z>LT[3],arr.ind=TRUE)-> zgt6
  which(z>LT[4],arr.ind=TRUE)-> zgt7
  plot(y=100, x= -1,xlim=c(max(ppm), min(ppm)),ylim=c(1,nm),xlab="Resonance",main=title,type="n",yaxt="n",ylab="Chrom postion")
  points(x=ppm[zgt4[,2]], y= zgt4[,1],col="green",pch=22,bg="green",cex=0.2)
  points(x=ppm[zgt5[,2]], y= zgt5[,1],col="blue",pch=22,bg="blue",cex=0.2)
  points(x=ppm[zgt6[,2]], y= zgt6[,1],col="red",pch=22,bg="red",cex=0.2)
  points(x=ppm[zgt7[,2]], y= zgt7[,1],col="yellow",pch=22,bg="yellow",cex=0.2)
  midpoints=1:length(levels(res[,1]))
  chrchanges=1:length(levels(res[,1]))+1
  prev=0
  step=0
  start=0
  chr="1"
  k=1
  i=1
  chrchanges[1]=0
  while(i <= nm){
    if(res[i,1]!=chr){
      midpoints[k]=step+(prev-start)/2
      step=prev
      k=k+1
      start=i
      chrchanges[k]=start-0.5
      chr=res[i,1]
    }
    prev=i
    i=i+1
  }

## modif: if only one chr

if(length(levels(res[,1]))==1)
{
  midpoints=(prev-start)/2+step
}else{
 midpoints[k]=(prev-start)/2+step
}

  chrchanges[k+1]=nm

  axis(side=2,at=midpoints,labels=levels(res[,1]), tick=FALSE, line=-0.2, lty=6);
  axis(side=2,at=1+chrchanges,labels=FALSE)
  rug(ppm,ticksize=0.01,quiet=TRUE)
  
}
