drawspline <-
function (cn1, cn2, lwd = 1, col = "blue", ...) 
{
    x <- cbind(cn1[1],cn2[1])
    y <- cbind(cn1[2],cn2[2])
    r <- xspline(x, y, lty = 1, shape = 1, lwd = lwd, border = col, 
        ...)
}
