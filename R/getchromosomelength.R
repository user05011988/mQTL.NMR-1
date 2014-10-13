getchromosomelength <-
function (result, chr) 
{
    l <- ceiling(max(result[which(result[, 1] == chr), 2]))
    l
}
