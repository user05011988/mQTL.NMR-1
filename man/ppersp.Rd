\name{ppersp}
\alias{ppersp}
\title{
Plot a 3-D profile of LODs
}
\description{
Plot 3-D profile of LODs as function of genomic position and chemical shift   
}
\usage{
ppersp(z, ppm, title, theta=-15, phi=15, r=50)
}
\arguments{
  \item{z}{
a matrix specifying metabolome genome-wide mQTL mapping results
}
  \item{ppm}{
a vector of chemical shift
}
  \item{title}{
plot title
}
  \item{theta}{
angle defining the viewing direction (azimuthal direction)
}

  \item{phi}{
angle defining the viewing direction (colatitude direction)
}
  \item{r}{
the distance of the eyepoint from the centre of the plotting box.
}

}
\value{
plot 2D-profile
}
\author{
Jean-Baptiste Cazier
}

\seealso{
\code{\link{pplot}}
}

\examples{

# Download data files
load_datafiles()

# mQTL mapping results
load(results)

## Plot 3D profile
dev.new(width=5,height=5,pointsize=5)
ppersp(results$res, results$ppm, title="Example plot")
}

\keyword{ function }

