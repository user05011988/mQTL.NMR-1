\name{pplot}
\alias{pplot}
\title{
Plot a color scale layer
}
\description{
Plot the results with a color scale y layer over 3 in 2D
}
\usage{
pplot(z, title, ppm, res, LT = c(5,10,15,20))
}
\arguments{
  \item{z}{
a matrix specifying metabolome genome-wide mQTL mapping results
}
  \item{title}{
figure title 
}
  \item{ppm}{
a vector of chemical shift
}
  \item{res}{
mQTL results to be plotted (scanone object)
}
  \item{LT}{
quantil(res,probs), res: matrix of mQTL mapping results and probs: vector of probabilities
}
}

\value{
plot of 2-D profile
}

\author{
Jean-Baptiste Cazier
}

\seealso{
\code{\link{ppersp}}
}

\examples{

# Download data files
load_datafiles()

# mQTL mapping results
load(results)

## Plot 3D profile

dev.new(width=5,height=5,pointsize=5)

probs=c(0.95,0.99,0.999,0.9999) ## probabilities

pplot(results$res,"Full 2D Profile", results$ppm, results$best, quantile(results$res,probs=probs))

}

\keyword{function}

