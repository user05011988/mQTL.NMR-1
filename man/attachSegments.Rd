\name{attachSegments}
\alias{attachSegments}
\title{
Concatenation of test and reference segments
}
\description{
Concatenation of test and reference segments to ensure one-to-one
correspondence. 
}
\usage{
attachSegments(refSegments,testSegments)
}

\arguments{
  \item{refSegments}{
  a list characterizing the segments of the reference spectrum (start, end, peaks, center)
}
  \item{testSegments}{
    a list characterizing the segments of the test spectrum (start,end, peaks, center)
} 
}

\details{
The algorithm:
\enumerate{
\item{For each reference segment within segment boundaries, i.e. between
      initial and final positions, find all centre (middle) positions of test segments 
      and merge those segments, if more than one centre position is found}
\item{Apply the same procedure for each test segment}
}
}

\value{
A list:
\item{segments$start}{a vector specifiying the starting of each concatenated test segment}
\item{segments$PeakLeftBoundary}{a list defining the peak left boundary of each concatenated test segment}
\item{segments$PeakRightBoundary}{a list defining the peak right boundary of each concatenated test segment}
\item{segments$Peaks}{a list specifiying the peaks information of each concatenated test segment (max position, start position, end position,...)}
\item{segments$end}{a vector specifiying the end of each concatenated test segment}
\item{segments$end}{a vector specifiying the center of each concatenated test segment}
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
\code{\link{matchSegments}}
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

}

\keyword{function}

