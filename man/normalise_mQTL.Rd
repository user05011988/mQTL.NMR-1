\name{normalise_mQTL}
\alias{normalise_mQTL}

\title{
Normalisation of metabolomic data
}
\description{
Takes use of the base function \code{\link{normalise}} to provide a normalised metabolomic data file.
}
\usage{
normalise_mQTL(infile,outfile,method,refIdx=1, noiseInt=c(11,12))
}

\arguments{
  \item{infile}{
a text file with non-normalised spectra profiles
}
  \item{outfile}{
a text file with normalised spectra profiles 
}
  \item{method}{
a character defining the normalization method:
- Constant sum normalisation (method<-'CS')
- Constant noise normalisation (method<-'CN')
- Qoutient probabilistic method (method<-'PQN')
- Linear baseline normalisation (method<-'LBN')
- Auto-scaling (method<-'AS')
- Pareto scaling (method<-'PS')
}
  \item{refIdx}{
index of reference individual (set by the user) 
}
  \item{noiseInt}{
noise region on the resonance axis as an interval (ex. [11,12] ppm) 
}
}

\value{
a file containing normalised spectra profiles
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{normalise}}
}
\examples{

# Download data files
load_datafiles()

# Format data

format_mQTL(phenofile,genofile,physiodat,cleandat,cleangen)

# Constant Sum normlisation
nmeth<-'CS'
normalise_mQTL(cleandat,CSnorm,nmeth)

}
\keyword{ methods }

