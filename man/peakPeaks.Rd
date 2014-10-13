\name{peakPeaks}
\alias{peakPeaks}
\title{
Peak picking algorithm
}
\description{
Identification of peaks in metabolomic data based on the calculation of smoothed derivates using Savitzky-Golay filter. The peak is identified if derivative crosses zero, i.e. sign(X'(i))>sing(X'(i+1)).
}
\usage{
peakPeaks(SpSmooth, dpDerivs, Sp)
}
\arguments{
  \item{SpSmooth}{
a vector specifying smoothed spectrum
}
  \item{dpDerivs}{
a vector specifying smoothed derivative of the spectrum
}
  \item{Sp}{
a vector specifying the spectrum of intrest
}
}
\value{
identified peaks
}
\references{Veselkov,K. et al (2009) Recursive Segment-Wise Peak Alignment of Biological 1H
NMR Spectra for Improved Metabolic Biomarker Recovery, Anal. Chem., 81(1),
56-66.
}
\author{
Lyamine Hedjazi
}
\seealso{
\code{\link{sgolayDeriv}}
}
\examples{

load_datafiles()
Sp<-t(read.table(phenofile))

## Peak picking
Spectrum<-Sp[1,]
iOrder <- 3
iFrameLen<- 11

SpDerivs<-sgolayDeriv(Spectrum,iOrder,iFrameLen,2)
SpSmooth<-sgolayDeriv(Spectrum,iOrder,iFrameLen,1)
peaks<-peakPeaks(SpSmooth,SpDerivs,Spectrum)
}

\keyword{methods}
