validatePeaks <-
function(SpSmooth,peaks,peakParam) 
{
# input:          Peak picking details
#                 peaks:  maxPos - peak maxium position
#                         startPos - start position
#                         endPos - end position
#                         maxVal - maximum value
#                         startVal - start value
#                         endVal - end value
#                         basl - baseline value
#                         index - peak index
########################################################################
#             Peak validation parameters
#             peakParam:  minPeakWidth - minimum peak width
#             ampThr - amplitude threshold automatically determined if it
#             is zero
# Author, L. Hedjazi, ICAN 2013

peakCount<-nrow(peaks)
# Matrix pre-location
validatedPeaks<-peaks
minPeakWidth<-peakParam$minPeakWidth

if (("ampThr" %in% names(peakParam))==FALSE|| peakParam$ampThr==FALSE){
    ampThr<-getAmpThr(peaks)
    peakParam$ampThr<-ampThr
}else{
    ampThr<-peakParam$ampThr}

if ((peakParam$ampThr>(0.9*max(SpSmooth)))||(peakParam$ampThr<(1.1*min(SpSmooth)))) {
    stop('Peak validation threshold exceeds spectrum maximum and minimum values')}

index<-1

for (i in 1:peakCount) 
{
    if (((peaks$endPos[i]-peaks$startPos[i]) > minPeakWidth) && (peaks$maxVal[i]-peaks$basl[i] > ampThr)) {
 
        validatedPeaks[index,]<-peaks[i,]
        index<-index+1}
}

if (index> 1)
 {
    PeakCount<-index-1
        validatedPeaks<-validatedPeaks[1:PeakCount,]
}else{
    stop('wrong peak peaking parameters: No Validated peaks')
}


minsegwidth<-1e10
for (i in 1:PeakCount)
{
    startPos<-validatedPeaks$startPos[i]
    maxPos<-validatedPeaks$maxPos[i]
    endPos<-validatedPeaks$endPos[i]
    segwidth<-endPos-startPos
    # Determine the peak boundaries
    edgeVal<-validatedPeaks$maxVal[i]*peakParam$peakEdgeMax

    tstleft<-which((SpSmooth[startPos:maxPos]-validatedPeaks$basl[i])>= edgeVal)
     if (length(tstleft)==0)
      {
        validatedPeaks$LeftEdge[i]<-startPos
    }else{
        LeftEdge<-which((SpSmooth[startPos:maxPos]-validatedPeaks$basl[i])>= edgeVal)[[1]]
        validatedPeaks$LeftEdge[i]<-startPos+LeftEdge-1
    }
    
    tstright<-which(SpSmooth[maxPos:endPos]-validatedPeaks$basl[i]>= edgeVal)
    
    if (length(tstright)==0)
     {
        validatedPeaks$RightEdge[i]<-endPos
    }else{
        RightEdge<-tail(which(SpSmooth[maxPos:endPos]-validatedPeaks$basl[i]>= edgeVal),n=1)
        validatedPeaks$RightEdge[i]<-maxPos+RightEdge-1
    }

    if (minsegwidth>segwidth) 
    {minsegwidth<-segwidth}
}

assign("peakParam", peakParam,envir= parent.frame())
assign("peaksValidated",validatedPeaks,envir=parent.frame())
assign("minsegwidth", minsegwidth,envir= parent.frame())
}
