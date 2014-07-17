validateSegments <-
function(refSegments,testSegments)
{
# Combine segments 
ref_length<-length(refSegments$Peaks)
int_length<-length(testSegments$Peaks)

i_centre<- get_central_pos(testSegments)
index<-1
segments<-list()
concatination<-list()
concatination$segments<-list()

for (i in 1:ref_length)
{
    left_bnd<-refSegments$start[i]
    right_bnd<-refSegments$end[i]
    ind<-which((i_centre>left_bnd) & (i_centre<right_bnd))

    if (length(ind)>1)
     {
        concatination$segments[[index]]<-ind
        index<-index+1
    }
}

if (index==1) 
{
segments<-testSegments
}else{

    conc_index<-1
    seg_index<-1
    i<-1
    while (i<=int_length)
     {
        if (conc_index<index)
         {
            if (concatination$segments[[conc_index]][1]==i)
             {
                indexes<-concatination$segments[[conc_index]]

                tstSg<-list()
                tstSg$start<-testSegments$start[indexes]
                tstSg$PeakLeftBoundary<-testSegments$PeakLeftBoundary[indexes]
                tstSg$PeakRightBoundary<-testSegments$PeakRightBoundary[indexes]
                tstSg$Peaks<-testSegments$Peaks[indexes]
                tstSg$end<-testSegments$end[indexes]
                tstSg$centre<-testSegments$centre[indexes]


                segment<-unite_segments(tstSg)

                #segments(seg_index)<-segment

                segments$start[seg_index]<-segment$start
                segments$PeakLeftBoundary[[seg_index]]<-segment$PeakLeftBoundary
                segments$PeakRightBoundary[[seg_index]]<-segment$PeakRightBoundary
                segments$Peaks[[seg_index]]<-segment$Peaks
                segments$end[seg_index]<-segment$end
                segments$centre[seg_index]<-segment$centre

                i<-tail(concatination$segments[[conc_index]],n=1)+1
                seg_index<-seg_index+1
                conc_index<-conc_index+1
                next
            }
       }
        #segments[seg_index]<-testSegments[i]
 
                segments$start[seg_index]<-testSegments$start[i]
                segments$PeakLeftBoundary[seg_index]<-testSegments$PeakLeftBoundary[i]
                segments$PeakRightBoundary[seg_index]<-testSegments$PeakRightBoundary[i]
                segments$Peaks[seg_index]<-testSegments$Peaks[i]
                segments$end[seg_index]<-testSegments$end[i]
                segments$centre[seg_index]<-testSegments$centre[i]

        i<-i+1
        seg_index<-seg_index+1
    }
}
return(segments)
}
