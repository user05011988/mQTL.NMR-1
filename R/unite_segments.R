unite_segments <-
function(segments)
{
# concatination of segments
ilength<-length(segments$start)
i_centre<-NULL
segment<-list()

segment$start<-segments$start[1]

for (i in 1:ilength)
{
    segment$PeakLeftBoundary<-c(segment$PeakLeftBoundary, segments$PeakLeftBoundary[[i]])
    segment$PeakRightBoundary<-c(segment$PeakRightBoundary,segments$PeakRightBoundary[[i]])
    segment$Peaks<-rbind(segment$Peaks,segments$Peaks[[i]])
}

segment$end<-tail(segments$end,n=1)

segment$centre<-ceiling((segment$start+segment$end)/2)
return(segment)
}
