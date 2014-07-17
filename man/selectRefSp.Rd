\name{selectRefSp}
\alias{selectRefSp}
\title{
Automated selection of a reference spectrum
}
\description{
The selection of reference spectrum among all spectrums is based on the highest similarity to all other spectra
}
\usage{
selectRefSp(X, step)
}
\arguments{
  \item{X}{
matrix of spectra
}
  \item{step}{
a numerical parameter used to scale spectral regions down to specific bin size
}
}
\value{
returns the index of selected spectrum
}

\author{
Lyamine Hedjazi
}
\seealso{
\code{\link{alignSp}}
}

\examples{

# Data

Sp=matrix(rnorm(10*5000,mean=0,sd=1), nrow=10,ncol=5000)

# Reference spectrum selection

step=0.02 # Recursion step (default 0.02)
index<-selectRefSp(Sp,step)

}

\keyword{ function }
