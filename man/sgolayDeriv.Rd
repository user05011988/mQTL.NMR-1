\name{sgolayDeriv}
\alias{sgolayDeriv}
\title{
Calculate smoothed derivates 
}
\description{
Calculate smoothed derivates using Savitzky-Golay filter 
}
\usage{
sgolayDeriv(dpSpectr, iOrder, iFrameLen, j)
}
\arguments{
  \item{dpSpectr}{
a vector specifying the input spectrum
}
  \item{iOrder}{
polynomial order of Savitzky - Golay filter
}
  \item{iFrameLen}{
Savitzky-Golay frame length in ppm scale
}
  \item{j}{
order of derivative
}
}
\value{
jth dervitative of the spectrum
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{sgolay}}
}

\examples{

## Data

Sp=matrix(rnorm(10*13454,mean=0,sd=1), nrow=10,ncol=13454)

## Peak picking
Spectrum<-Sp[10,]
iOrder <- 3
iFrameLen<- 11
j<-2

SpDerivs<-sgolayDeriv(Spectrum,iOrder,iFrameLen,j)

}

\keyword{ function }

