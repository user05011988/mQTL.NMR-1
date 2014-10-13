\name{setupRSPA}
\alias{setupRSPA}
\title{
setup of alignment parameters
}
\description{
Configuration of the RSPA algorithm invariant parameters 


}
\usage{
setupRSPA(ppm)
}
\arguments{
  \item{ppm}{
a vector defining chemical shift scale
}
}

\author{
Jean-Baptiste Cazier
}

\seealso{
 \code{\link{configureRSPA}} 
}

\examples{

load_datafiles()

load(results)
ppm<-results$ppm
setupRSPA(ppm)

}

\keyword{ function }


