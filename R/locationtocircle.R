locationtocircle <-
function (result, chr, loc, spacing = 50, fixoutofbounds = TRUE, 
    verbose = FALSE) 
{
    templateresult <- result
    totallength <- getgenomelength(result)
    nchr <- length(unique(templateresult[, 1]))
    cvalues <- circlelocations(totallength + (nchr * spacing))
    l <- 1
    for (x in unique(templateresult[, 1])) {
        if (x == chr) {
            if (loc < getchromosomelength(result, x)) {
                return(t(cvalues[(l + loc), ]))
            }
            else {
                if (verbose) 
                  cat("Location out of chromosome bounds", loc, 
                    " ", getchromosomelength(result, x), "\n")
                if (fixoutofbounds) 
                  return(t(cvalues[(l + getchromosomelength(result, 
                    x)), ]))
                stop(paste("Location out of chromosome bounds", 
                  loc, " ", getchromosomelength(result, x), "\n"))
            }
        }
        l <- l + getchromosomelength(result, x) + spacing
    }
    stop("No such chromosome")
}
