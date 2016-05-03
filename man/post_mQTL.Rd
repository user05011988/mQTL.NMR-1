\name{post_mQTL}
\alias{post_mQTL}
\title{
Plot top LOD results
}
\description{
plot the results of a given run 
}
\usage{
post_mQTL(results, probs = c(0.95, 0.99, 0.999, 0.9999))
}
\arguments{
  \item{results}{
a list containing the results of mQTL analysis.
}
  \item{probs}{
a numerical vector of probabilities with values in [0,1]. (Values up to 2e-14 outside that range are accepted and moved to the nearby endpoint).  
}
}
\details{
This function plots differents results corresponding to top LOD marker  
}
\value{
It returns one window gathering all figures of the mQTL analysis. Each figure is also saved separately in the user's working space.
}

\author{
Hedjazi Lyamine
}

\seealso{
\code{\link{pre_mQTL}}
}

\examples{
# Download data files
load_datafiles()

# mQTL mapping results
load(results)

# Plot mQTL mapping results
post_mQTL(results)
}

\keyword{ function }

