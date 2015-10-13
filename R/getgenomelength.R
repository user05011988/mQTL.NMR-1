getgenomelength <-
function (result) 
{
    l <- 1
    for (x in unique(result[, 1])) {
        l <- l + getchromosomelength(result, x)
    }
    l
}
