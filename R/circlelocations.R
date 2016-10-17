circlelocations <-
function (nt) 
{
    medpoints <- matrix(nrow = nt, ncol = 2)
    phi <- seq(0, 2 * pi, length = (nt + 1))
    complex.circle <- complex(modulus = 1, argument = phi)
    for (j in 1:nt) {
        medpoints[j, ] <- c(Im(complex.circle[j]), Re(complex.circle[j]))
    }
    medpoints
}
