\name{SRV_lod.plot}
\alias{SRV_lod.plot}
\title{
Plot top lod SRV clusters 
}
\description{
Plot all SRV clusters associated with the top lod locus 
}
\usage{
SRV_lod.plot(results, file, Th)
}
\arguments{
  \item{results}{
a list specifying the results of mQTL mapping  
}
  \item{file}{
a text file contains resulting clusters
}
  \item{Th}{
a numerical value of LOD threshold
}
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{SRV.plot}}
}
\examples{

load_datafiles()
load(results)

## Plot LOD profile
SRV_lod.plot(results,rectangle_SRV,T=1)

}
\keyword{ function }

