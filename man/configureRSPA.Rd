\name{configureRSPA}
\alias{configureRSPA}
\title{
segmentaion and recursive alignment parameters
}
\description{
The routine used to change and improve the RSPA algorithm performance

}
\usage{
configureRSPA(ppm)
}
\arguments{
  \item{ppm}{
a numerical vector defining the chemical shift scale
}
}

\author{
Jean-Baptiste Cazier
}

\seealso{
 \code{\link{setupRSPA}} 
}

\examples{

load_datafiles()

load(results)
ppm<-results$ppm
configureRSPA(ppm)

}

\keyword{ function }

