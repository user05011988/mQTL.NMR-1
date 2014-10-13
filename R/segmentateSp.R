segmentateSp <-
function(Sp,peakParam)
{
# Determination of highly intensive peaks in the spectrum of interest and 
# subsequent concatenation of closely located peaks into 
# larger segments 
# Algorithm: - smooth spectrum X using SG
#            - locate peak maxima when the first 
#              derivative crosses zero, i.e. sign(X'(i))>sing(X'(i+1))
#            - validate peaks (/or eliminate noisy peaks)
#            - concatenate closely located peaks into larger segments
# Input: 
#                             Sp - spectrum
# Peak parameters:  peakParam$                             
#                             ampThr - amplitude threshold   [default 2*median(peaksMaxValues)] 
#                             iFrameLen - Savitzky-Golay frame length
#                             iOrder - polynomial order of Savitzky - Golay filter
#                             minPeakWidth - min peak size
#                             ppmDist - distance to concatenate adjacent peaks
#Output:                     segments
#Author: L. Hedjazi, ICAN Paris 2013 

# Pre-location
peaksValidated<-list()

#perform Savitzkiy Golay smoothing
SpDerivs <- sgolayDeriv(Sp,peakParam$iOrder,peakParam$iFrameLen,2)
SpSmooth <- sgolayDeriv(Sp,peakParam$iOrder,peakParam$iFrameLen,1)

#indentify peaks
peaks<-peakPeaks(SpSmooth,SpDerivs,Sp)
#validate peaks
validatePeaks(SpSmooth,peaks,peakParam)
#locate segments
segments<-segmentate(Sp,peaksValidated,peakParam)

assign("peakParam", peakParam,envir = parent.frame())
return(segments)
}
