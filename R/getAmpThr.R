getAmpThr <-
function(peaks)
{
# Automatic determination of amplitude threshold for peak peaking
# based on the 5% of the most intensive peaks 
# Author, L. Hedjazi, ICAN Paris, 2013

PeakCount<-nrow(peaks)
peakMaxValues <- rep(NaN,PeakCount)

for (i in 1:PeakCount)
{
      peakMaxValues[i]<-peaks$maxVal[i]-peaks$basl[i]
}
### Select threshold based on 5% of the most intensive peaks

index<-floor(PeakCount*0.95)
peakSortedValuess<-sort(peakMaxValues)
ampThr<-peakSortedValuess[index]
return(ampThr)

}
