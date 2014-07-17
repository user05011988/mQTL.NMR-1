add_peak <-
function(data,gen,loc,amp,eff,s=1,sloc=1,samp=0.1,seff=0.1){
# Script to add a simulated peak
  nr=dim(data)[[1]]
  nc=dim(data)[[2]]
  for(r in 1:nr){
    if(gen[r]==9){
      g=sample(c(0,1,2))[1]
    }else{
      g=gen[r]
    }
   # data[r,]=data[r,]+abs(rnorm(1,amp,samp)+rnorm(1,eff,seff)*g)*dnorm(1:nc,rnorm(1,loc,sloc),s)
    sdamp<-abs(rnorm(1,amp,samp*amp)*(1+rnorm(1,eff,seff*eff))*g)
    data[r,]=data[r,]+sdamp*dnorm(1:nc,rnorm(1,loc,sloc),s)*s*sqrt(2*pi)
  }
#  print(paste(nr,nc,loc,amp,eff))
  return(data)
}
