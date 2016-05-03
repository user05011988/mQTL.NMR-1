ppmToPt <-
function(ppmValues, firstPtPpm, resolution){

if(nargs() < 2 | missing(firstPtPpm))
    {stop('nargs < 2 || missing(firstPtPpm)')}


if(nargs() < 3 | missing(resolution))
    {resolution <- ppmValues[2] - ppmValues[1]}

if(length(firstPtPpm)>1)
    {stop(paste('First ppm should be a number, got non-scalar value:', firstPtPpm))}

if(length(resolution)>1)
    {stop(paste('Resolution ppm should be a number, got non-scalar value:', resolution))}

ppmShift <- ppmValues - firstPtPpm

pt <- round(ppmShift / resolution) + 1


return(pt)

}
