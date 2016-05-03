segmentate <-
function(Sp, peaks, peakParam)
{
# Combination of adjacent peaks into larger segments
#  Input: SpSmooth - spectrum of interest
#
#                 peaks.  maxPos - peak maxium position
#                         startPos - starting position
#                         endPos - ending position
#                         maxVal - maximum value
#                         startVal - starting value
#                         endVal - ending value
#                         basl - baseline value
#                         index - peak index
#
#                 peakParam.ppmDist - distance to combine adjacent peaks
#
# Author: Lyamine Hedjazi, ICAN Paris, 2013

peakCount<-nrow(peaks)
segments<-list()
ppmDist<-peakParam$ppmDist

segmentIndex<-1
peakIndex<-1

while (peakIndex<=peakCount)
{

    segments$start[segmentIndex]<-peaks$startPos[peakIndex]
    segments$PeakLeftBoundary[segmentIndex]<-as.data.frame(peaks$LeftEdge[peakIndex])
    segments$PeakRightBoundary[segmentIndex]<-as.data.frame(peaks$RightEdge[peakIndex])
    #segments$Peaks<-rbind(segments$Peaks,peaks[peakIndex,])

    segments$Peaks[[segmentIndex]]<-peaks[peakIndex,]
    
    while (peakIndex<=peakCount)
     {
         # check whether the next peak is part of the same segment
        #TODO: optimise no matter to store PeakLeft(Right)Boundary if we
        #store segment peaks themeselves.
        includePeak <- (peakIndex<peakCount) && ((peaks$maxPos[peakIndex+1]-peaks$maxPos[peakIndex])<ppmDist)

        if (includePeak)
        {
          peakIndex<-peakIndex+1
          segments$PeakLeftBoundary[[segmentIndex]]<-c(segments$PeakLeftBoundary[[segmentIndex]], peaks$LeftEdge[peakIndex])
          segments$PeakRightBoundary[[segmentIndex]]<-c(segments$PeakRightBoundary[[segmentIndex]],peaks$RightEdge[peakIndex])
          segments$Peaks[[segmentIndex]]<-rbind(segments$Peaks[[segmentIndex]], peaks[peakIndex,])
          #segments$end[segmentIndex]<-NULL
        }else {
            segments$end[segmentIndex]<-peaks$endPos[peakIndex]
            segments$centre[segmentIndex]<-ceiling((segments$start[segmentIndex]+segments$end[segmentIndex])/2)
            segmentIndex<-segmentIndex+1
            peakIndex<-peakIndex+1
            break
        }
    }
}

segmentCount<-segmentIndex-1

segments$PeakLeftBoundary <-segments$PeakLeftBoundary[1:segmentCount]
segments$PeakRightBoundary<-segments$PeakRightBoundary[1:segmentCount]
segments$Peaks<-segments$Peaks[1:segmentCount]
segments$end<-segments$end[1:segmentCount]
segments$centre<-segments$centre[1:segmentCount]

segmentVld<-segments

 for (i in 1:segmentCount)
  {
        segmentVld$start[i]<-min(segmentVld$start[i], segmentVld$PeakLeftBoundary[[i]])
        segmentVld$end[i]<-max(segmentVld$end[i], segmentVld$PeakRightBoundary[[i]])
  }

return(segmentVld)

}
