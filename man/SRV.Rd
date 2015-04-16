\name{SRV}
\alias{SRV}
\title{
Statistical Recoupling of Variables
}
\description{
Base function for dimensionality reduction by statistical recoupling of variables
}
\usage{
SRV(X, minsize, correl, clustf = median)
}
\arguments{
  \item{X}{
matrix of metabolomic data
}
  \item{minsize}{
a numerical value defining the singlet size
}
  \item{correl}{
a numerical value defining the bucketting resolution
}
  \item{clustf}{
a numerical value defining the correlation threshold
}
}

\value{
A list:
\item{indicesdebf}{a vector indicating the starting border of superclusters}
\item{indicesfinf}{a vector indicating the ending border of superclusters}
\item{Xcluster}{matrix of reduced data}
}
\references{
Blaise,B. et al (2009) Statistical recoupling prior to significance testing in nuclear magnetic resonance based metabonomics, Anal. Chem., 81(15), 6242-6251.
}
\author{
Jean-Baptiste Cazier
}
\seealso{
\code{\link{pre_mQTL}}
}
\examples{

# Load data files

    load_datafiles()

    Sp<-read.table(phenofile, as.is=TRUE, header=TRUE, sep='\t')

# Perform the SRV analysis to reduce the number of dimension of Spectra #data (Sp)

    corrT=0.9 # correlation threshold
    minsize=10 # singlet size
    met="rectangle" # summary measure

    SRV<-SRV(t(Sp), minsize, corrT,clustf=met)
}

\keyword{ methods }
