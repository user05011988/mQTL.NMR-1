peakPeaks <-
function(SpSmooth,dpDerivs,Sp)
{
# Peak peaking:
# Input: SpSmooth - smoothed spectrum
#        dpDerivs - smoothed derivative of the spectrum

# - the peak is identified if derivative crosses zero,
# i.e. sign(X'(i))>sing(X'(i+1))

# Author Lyamine Hedjazi, ICAN 2013


iSpecLen<-length(SpSmooth)
iPeakInd <-1

# Pre-location
peaks<-list()
#peaks$maxPos<-rep(NaN,iSpecLen)

for (i in (1:(iSpecLen-1)))
{
    # coarse peak maximum position
    if ((dpDerivs[i]>=0)&& (dpDerivs[i+1]<0)) 
     {
        peaks$maxPos[iPeakInd]<-i+1
        # Temporary starting and ending peak positions
        iPeakInd<-iPeakInd+1
    }
}

peakCount<-iPeakInd-1
peaks$maxPos<-peaks$maxPos[1:peakCount]

targetPkIdx<-1

for (srcPkIdx in 1:peakCount)
{
    maxPos<-peaks$maxPos[srcPkIdx]
    
    while ((maxPos > 2) && (maxPos < (iSpecLen-2)))
     {
        if (SpSmooth[maxPos-1]<=SpSmooth[maxPos]&&SpSmooth[maxPos]>=SpSmooth[maxPos+1]) 
        {
            
           if (targetPkIdx > 1 && peaks$maxPos[targetPkIdx-1]==maxPos) 
           {
                # the same maximum value - just skip it
                break
           }
            # save the new index:
            peaks$maxPos[targetPkIdx]<- maxPos
            targetPkIdx <- targetPkIdx + 1
            break
         }
        if (SpSmooth[maxPos]<=SpSmooth[maxPos+1])
         {
            maxPos<-maxPos+1
        }else { 
            if(SpSmooth[maxPos]<=SpSmooth[maxPos-1])
            {maxPos<-maxPos-1}
        }
    }
}

peakCount<-targetPkIdx-1
peaks$maxPos<-peaks$maxPos[1:peakCount]


for (i in 1:peakCount)
  {
    j<-peaks$maxPos[i]
    k<-peaks$maxPos[i]

    # left boundary
    while ((SpSmooth[j]>=SpSmooth[j-1]) && (j-1!=1)) #first index
        {j<-j-1}

    # right boundary
    while ((SpSmooth[k]>=SpSmooth[k+1]) && (k+1 != iSpecLen)) #last index
        {k<-k+1}


    peaks$startPos[i]<-j
    peaks$endPos[i]<-k
    peaks$centre[i]<-ceiling((k+j)/2)
    peaks$startVal[i]<-SpSmooth[j]
    peaks$endVal[i]<-SpSmooth[k]
    peaks$index[i]<-i
    #Use peak maximum position from original spectrum
    #instead of smoothed one.
    peaks$maxVal[i]<-max(Sp[j:k])
    maxInd<-which.max(Sp[j:k])
    peaks$maxPos[i]<-j+maxInd-1

    #estimate the baseline as minimum value:
    peaks$basl[i] <- min(cbind(SpSmooth[k], SpSmooth[j]))
}

peaks<-as.data.frame(peaks)
return(peaks)
}
