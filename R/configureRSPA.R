configureRSPA <-
function(ppm){

recursion<-list()
peakParam<-list()

################Recursion minimum segment size##################
recursion$minSegWidth<-0.01 #(ppm) Stop criteria of the recursion - the size of the smallest peak
recursion$step<-0.02 #(ppm) - used for calculation of variance-scaled CC
#################### Peak peaking parameters ###############################
peakParam$minPeakWidth <- 0.005 #min peak width in ppm scale
peakParam$iFrameLen<-0.005 #Savitzky-Golay frame length in ppm scale
peakParam$iOrder<-3 #polynomial order of Savitzky - Golay filter
peakParam$peakEdgeMax<-0.2 
########### Matching parameters ############################################
MAX_DIST_FACTOR<-0.5 # The distance matching parameter (0.5*peak_width)
MIN_RC<-0.25 # Minimum resamblance coefficient

if (!missing(ppm)) {
    peakParam$minPeakWidth<-ppmToPt(peakParam$minPeakWidth,0,ppm[2]-ppm[1])
    peakParam$iFrameLen<-ppmToPt(peakParam$iFrameLen,0,ppm[2]-ppm[1])
    recursion$minSegWidth<-ppmToPt(recursion$minSegWidth,0,ppm[2]-ppm[1])
    recursion$step<-ppmToPt(recursion$step,0,ppm[2]-ppm[1])}

assign("peakParam", peakParam,envir= parent.frame())
assign("recursion", recursion,envir= parent.frame())
assign("MAX_DIST_FACTOR", MAX_DIST_FACTOR,envir = parent.frame())
assign("MIN_RC", MIN_RC,envir = parent.frame())
}
