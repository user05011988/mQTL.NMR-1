get_central_pos <-
function(segments)
{
# segments 
ilength<-length(segments$start)
i_centre<-segments$centre[1:ilength]

return(i_centre)
}
