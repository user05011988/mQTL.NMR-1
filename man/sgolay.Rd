\name{sgolay}
\alias{sgolay}
\title{
Find the matrix of differentiation filters
}

\description{
designs a Savitzky-Golay (polynomial) FIR smoothing filter. The polynomial order must be less than the frame size which must be odd.  
}

\usage{
sgolay(k,F,W)
}
\arguments{
  \item{k}{
 a numerical value of polynomial order
}
  \item{F}{
 a numerical value of frame size
}
  \item{W}{
 weighting matrix
}
}
\value{
matrix of differentiators
}
\references{
Sophocles J. Orfanidis, INTRODUCTION TO SIGNAL PROCESSING, Prentice-Hall, 1995, Chapter 8
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{sgolayDeriv}}
}
\examples{
k <- 3
F <- 11

Sg=sgolay(k,F)
}

\keyword{ function }

