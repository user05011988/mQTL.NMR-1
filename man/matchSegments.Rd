\name{matchSegments}
\alias{matchSegments}
\title{
Matching the segment of interest to the corresponding reference 
}
\description{
The algorithm makes use of a fuzzy logic approach to match the segment of interest to the corresponding reference
}
\usage{
matchSegments(refSp, intSp, intSegments, refSegments, MAX_DIST_FACTOR, MIN_RC)
}
\arguments{
  \item{refSp}{
a vector specifying the spectrum of reference
}
  \item{intSp}{
a vector specifying the spetcrum of interest (test spectrum)
}
  \item{intSegments}{
a list characterizing the segments of spectrum of interest
}
  \item{refSegments}{
a list characterizing the segments of the reference spectrum (start, end, peaks, center) 
}
  \item{MAX_DIST_FACTOR}{
distance matching parameter (0.5*peak_width)
}
  \item{MIN_RC}{
minimum resamblance coefficient
}
}

\details{
Algorithm:
\enumerate{
\item{pick-up segment of interest}
\item{pick-up reference segments}
\item{calculate relative distance between them}
\item{calculate relative resamblance between them}
\item{find min value of relative distance and resamblance}
\item{use it as representative of similiarity between target and reference segments}
\item{find the segment that has the highest value of both relative distance and resamblance}
}
}

\value{
A list:
\item{testSegs}{a list characterizing the matched test segments}
\item{refSegs}{a list characterizing the matched reference segments}
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
\code{\link{attachSegments}}
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

##segmentate a test spectrum
testSegments<- segmentateSp(Sp[1,], peakParam) # segmentate test spectrum (1st sample)

##attach test and reference segments
attachedSegs<-attachSegments(refSegments,testSegments)

##Match test and reference segments
attach(attachedSegs)
Segs<-matchSegments(refSp,Sp[1,],testSegmentsNew,refSegmentsNew,MAX_DIST_FACTOR, MIN_RC)

}

\keyword{function}
