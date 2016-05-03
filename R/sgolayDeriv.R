sgolayDeriv <-
function(dpSpectr,iOrder,iFrameLen,j) 
{
# Calculate smoothed derivates using Savitzky - Golay filter
# iFrameLen- the length of frame window

if (nargs()<1) {stop('Incorrect number of input arguments')}

if (nargs()<2) {iOrder <- 3}

if (nargs()<3) {iFramLen<-11}

if (nargs()<4) {j<-2} #Derivative

iFrameLen<-(floor(iFrameLen/2))*2+1 # iFramLen must be odd

iSpecLen <- length(dpSpectr)

g<- sgolay(iOrder,iFrameLen)

#dpDerivs[1:iFrameLen]<- 0
dpDerivs<-as.vector(rep(0,iFrameLen))
dpDerivs[(iSpecLen-((iFrameLen+1)/2)):iSpecLen]<-0

for (n in ((iFrameLen+1)/2):(iSpecLen-(iFrameLen+1)/2)){
    #calculate first order derivate
    dpDerivs[n]<-(t(g[,j]) %*%(dpSpectr[(n - ((iFrameLen+1)/2)+ 1): (n + ((iFrameLen+1)/2) - 1)]))   
    }
 return(dpDerivs)
}
