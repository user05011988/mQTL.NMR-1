\name{SRV.plot}
\alias{SRV.plot}
\title{
Plot SRV clusters 
}
\description{
Plot arrows defined by SRV on data
}
\usage{
SRV.plot(file1,file2,lo,hi,k,title)
}

\arguments{
  \item{file1}{
a text file with NMR data
}
  \item{file2}{
a text file with SRV results
}
  \item{lo}{
starting point on chemical shift
}
  \item{hi}{
ending point on chemical shift
}
  \item{k}{
number of samples
}
  \item{title}{
title of the plot
}
}

\author{
Lyamine Hedjazi 
}

\seealso{
\code{\link{simple.plot}}
}
\examples{

# Load data files

load_datafiles()

# Format data

format_mQTL(phenofile,genofile,physiodat,cleandat,cleangen)

## Plot SRV profile

SRV.plot(file1=cleandat,file2=rectangle_SRV,lo=3.02,hi=3.08,k=1:168,title="Cluster plot")

}

\keyword{ function }
