attachSegments <-
function(refSegments,testSegments)
{
#Concatenation of test and reference segments to their ensure one-to-one
#correspondence
#Algorithm
#    - For each reference segment within segment boundaries, i.e. between
#      initial and final positions, find all centre (middle) positions of test segments 
#      and merge those segments, if more than one centre position is found
#    - Apply the same procedure for each test segment
#    Input: refSegments
#           testSegments

testSegmentsNew<-validateSegments(refSegments,testSegments)
refSegmentsNew<-validateSegments(testSegmentsNew,refSegments)

return(list(testSegmentsNew=testSegmentsNew,refSegmentsNew=refSegmentsNew))
}
