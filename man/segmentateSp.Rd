\name{segmentateSp}
\alias{segmentateSp}
\title{
Segmentation of a spectrum of interest
}
\description{
Determination of highly intensive peaks in the spectrum of interest and 
subsequent concatenation of closely located peaks into 
larger segments
}
\usage{
segmentateSp(Sp, peakParam)
}
\arguments{
  \item{Sp}{
a vector defining the spectrum
}
  \item{peakParam}{
a list:
\itemize{
  \item{ampThr: amplitude threshold   [default 2*median(peaksMaxValues)] }
  \item{iFrameLen: Savitzky-Golay frame length }
  \item{iOrder: polynomial order of Savitzky - Golay filter}
  \item{iFrameLen: Savitzky-Golay frame length }
  \item{minPeakWidth: min peak size}
  \item{ppmDist: distance to concatenate adjacent peaks}
}
}
}

\value{
A list:
\item{testSegmentsNew}{a list specifying the new test segments}
\item{refSegmentsNew}{a list specifying the new reference segments}
}
\references{
Veselkov,K. et al (2009) Recursive Segment-Wise Peak Alignment of Biological 1H
NMR Spectra for Improved Metabolic Biomarker Recovery, Anal. Chem., 81(1),
56-66.
}
\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{attachSegments}}, \code{\link{matchSegments}}
}

\examples{

## Data
load_datafiles()
Sp<-t(read.table(phenofile))
ppm<-as.numeric(colnames(Sp))

## Normalization
normSp<-normalise(abs(Sp),'CS')

##Segmentation and matching parameters
setupRSPA(ppm)

##reference spectrum selection
attach(normSp)
index<-selectRefSp(Sp,recursion$step)
refSp<-Sp[index,]

##segmentate a reference spectrum
refSegments<- segmentateSp(refSp, peakParam) # segmentate reference spectrum
}

\keyword{ methods }
