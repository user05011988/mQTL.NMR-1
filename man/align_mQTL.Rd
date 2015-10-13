\name{align_mQTL}
\alias{align_mQTL}
\title{
Peak alignment and normalisation of metabolomic data
}
\description{
Recursive Segment-Wise Peak Alignment (RSPA) for accounting 
peak position
variation across metabolomic data
}
\usage{
align_mQTL(datafile, outdat,idx)
}
\arguments{
  \item{datafile}{ 
  The main input file of raw spectra in the csvs format 
}
  \item{outdat}{
  The output file of aligned spectra in the csvs format
}
  \item{idx}{
  index of reference spectrum
}
}
\details{
The algorithm is based on the following workflow:
\enumerate{
  \item{Automatic selection of a reference spectrum (if required).}
  \item{Segmentate a reference spectrum.}
  \item{Then for each test spectrum:
    \itemize{
      \item{segmentate a test spectrum.}
      \item{match test and reference segments.}
      \item{align a test spectrum.}
    }
  }
}
}

\value{
It returns a file with aligned data in the csvs format. 
}
\references{
Veselkov,K. et al (2009) Recursive Segment-Wise Peak Alignment 
of Biological 1H NMR Spectra for Improved Metabolic Biomarker 
Recovery, Anal. Chem., 81(1), 56-66.
}
\author{
Lyamine Hedjazi
}

\seealso{
 \code{\link{alignSp}}, \code{\link{attachSegments}}, \code{\link{matchSegments}}, 
\code{\link{segmentateSp}},\code{\link{format_mQTL}}, \code{\link{format_mQTL}} 
}

\examples{

# Download data files

load_datafiles()

# Format data

format_mQTL(phenofile,genofile,physiodat,cleandat,cleangen)

# Constant Sum normlisation
nmeth<-'CS'
normalise_mQTL(cleandat,CSnorm,nmeth)


# Alignment
align_mQTL(CSnorm,aligdat)

}
\keyword{methods}