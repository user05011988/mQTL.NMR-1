NMR.plot <-
function(data,ppm,x=5000,t=2000,k=1,ylim=range(data[k,x:(x+t)]),title="NMR profile"){
 # Plot the region of size t, starting at x


  y=-min(data[k,x:(x+t)])/10;
  if (length(k)==1){
    plot(ppm[x:(x+t)],data[k,x:(x+t)],type="l",col=4,xlim=c(ppm[x+t],ppm[x]), ylim=ylim,xlab="resonance, ppm",ylab="Intensity",main=title) ; 
  }else{
    plot(ppm[x:(x+t)],data[1,x:(x+t)],type="l",col=1,xlim=c(ppm[x+t],ppm[x]),ylim=ylim,xlab="resonance, ppm",ylab="Intensity",main=title) ; 
    j=2
    for (i in k[-1]){
      points(ppm[x:(x+t)],data[i,x:(x+t)],type="l",col=j) ; 
      j=j+1 %% 8
    }
  }
}
