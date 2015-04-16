\name{simple.plot}
\alias{simple.plot}
\title{
Plot NMR profile plus SRV regions
}
\description{
Plot NMR profile plus SRV regions and consensus across the various statistics
}
\usage{
simple.plot(file,lo,hi,k,title)
}
\arguments{
  \item{file}{
a text file containing NMR data
}
  \item{lo}{
starting point on the chemical axis
}
  \item{hi}{
ending point on the chemical axis
}
  \item{k}{
number of samples
}
  \item{title}{
title of the plot
}
}
\value{
NMR profile and SRV region plot with peak calling consensus
}

\author{
Jean-Baptiste Cazier
}

\seealso{
\code{\link{SRV.plot}}
}
\examples{

# Load data files

load_datafiles()

# Format data

format_mQTL(phenofile,genofile,physiodat,cleandat,cleangen)

# Plot NMR profile
 simple.plot(file=cleandat,lo=3.02,hi=3.08,k=1:20,title="NMR profile") 
}
\keyword{ function }

