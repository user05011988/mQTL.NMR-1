drawcirculargenome <-
function (result, lodmarkers = FALSE, spacing = 50) 
{
    result <- mqmextractmarkers(result)
    plot(c(-1.1, 1.1), c(-1.1, 1.1), type = "n", axes = FALSE, 
        xlab = "", ylab = "")
    totallength <- getgenomelength(result)
    nchr <- length(unique(result[, 1]))
    cvalues <- circlelocations(totallength + (nchr * spacing))
    l <- 1
    for (x in unique(result[, 1])) {
        nl <- l + getchromosomelength(result, x)
        lines(cvalues[l:nl, ], cex = 0.01)
        l <- nl + spacing
    }
    for (x in 1:nrow(result)) {
        if (lodmarkers) {
            points(locationtocircle(result, result[x, 1], result[x, 
                2], spacing = spacing), pch = 20, cex = min(c((result[x, 
                3]), 4)))
        }
        else {
            points(locationtocircle(result, result[x, 1], result[x, 
                2], spacing = spacing), pch = 20)
        }
    }
    for (x in 1:nchr) {
        chrnumberloc <- locationtocircle(result, unique(result[, 1])[x], getchromosomelength(result, 
            unique(result[, 1])[x])/2, spacing = spacing)
        points(t(c(-1.1, -1.15)))
        points(t(c(-0.9, -1.15)))
       #points(t(c(-0.7, -1.15)))
        text(t(c(-1, -1)), paste("Distances in cM"), cex = 0.6)
        text(t(c(-1.1, -1.1)), paste("0 cM"), cex = 0.6)
        text(t(c(-0.9, -1.1)), paste(round((totallength + (nchr * 
            spacing)) * (0.2/(2 * pi)), digits = 1), "cM"), cex = 0.6)
        #text(t(c(-0.7, -1.1)), paste(round((totallength + (nchr * 
            #spacing)) * (0.4/(2 * pi)), digits = 1), "cM"), cex = 0.6)
        text(1.04 * chrnumberloc, paste(unique(result[, 1])[x]), cex = 0.5)
    }
}
