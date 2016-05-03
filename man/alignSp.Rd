\name{alignSp}
\alias{alignSp}
\title{
Base function for Spectrum Alignment
}
\description{
Alignment of spectrum segement to the spectrum of interest
}
\usage{
alignSp(refSp, refSegments, intSp, intSegments, recursion, MAX_DIST_FACTOR, MIN_RC)
}
\arguments{
  \item{refSp}{
a vector specifying the reference spectrum
}
  \item{refSegments}{
a list characterizing the reference segments (start, end, peaks, ...)
}
  \item{intSp}{
a vector specifiying the spectrum of intrest
}
  \item{intSegments}{
a list characterizing the segment of interest (start, end, peaks, ...)
}
  \item{recursion}{
A list defining defaut values of the parameters of recursive alignment(minimal segment width, recursion step, resamblance, acceptance, ...) 
}
  \item{MAX_DIST_FACTOR}{
distance matching parameter (0.5*peak width)
}
  \item{MIN_RC}{
minimum resamblance coefficient
}
}

\value{
\item{alignedSpectrum}{aligned spectrum as a vector}
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{align_mQTL}}
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
refSegments<- segmentateSp(refSp, peakParam) 

##segmentate a test spectrum
testSegments<- segmentateSp(Sp[1,], peakParam) 

##attach test and reference segments
attachedSegs<-attachSegments(refSegments,testSegments)

##Match test and reference segments
attach(attachedSegs)
Segs<-matchSegments(refSp,Sp[1,],testSegmentsNew,refSegmentsNew,MAX_DIST_FACTOR, MIN_RC)

##Align test spectrum
attach(Segs)
SpAlg<- alignSp(refSp,refSegs,Sp[1,],testSegs,recursion,MAX_DIST_FACTOR,MIN_RC)

}

\keyword{ function }
