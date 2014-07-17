\name{pre_mQTL}
\alias{pre_mQTL}
\title{
Statistical Recoupling of variables for mQTL analysis
}
\description{
Makes use of SRV to preprocess metabolomic data for dimensionality reduction by statistical recoupling of variables
}
\usage{
pre_mQTL(infile, outfile, RedMet="SRV", met="sum", corrT = 0.9, BinWidth=0.01)
}
\arguments{
  \item{infile}{
metabolomic datafile in csvs format
}
  \item{outfile}{
reduced metabolomic datafile in csvs format
}

  \item{met}{
a charcater specifying the used statistical summary
}
  \item{RedMet}{
a charcater indicating the used dimensionality reduction method: Redmet="SRV" for statistical recoupling of variables and Redmet="bin" to apply the bining approach
}
  \item{corrT}{
a numerical parameter indicating correlation threshold
}
  \item{BinWidth}{
a numerical parameter indicating the bining width
}
}
\details{
mQTL-NMR package implements two dimensionality reduction methods.
The first one concerns the SRV algorithm which forms clusters of variables using a measure of a local spectral dependency. The second one concerns the classical bining method which divides the spectra into evenly spaced windows (bins) whose width commonly ranges between 0.001 and 0.05 ppm. 
}
\value{
  variables are associated into a series of clusters (or bins). This function provides in output the paramaters of the clusters (min and max borders, mean,...)
}


\references{
- Blaise,B. et al (2009) Statistical recoupling prior to significance testing in nuclear magnetic resonance based metabonomics, Anal. Chem., 81(15), 6242-6251.
- S praul, M.; Neidig, P.; Klauck, U.; Kessler, P.; Holmes, E.; Nicholson, J. K.; Sweatman, B.C.; Salman, S.R.; Farrant, R.D.; Rahr, E.; et al. J.Pharm. Biomed. Anal. 1994, 12, 1215-1225.
}
\author{
Lyamine Hedjazi
}
\seealso{
\code{\link{SRV}},\code{\link{post_mQTL}}
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

# Dimensionality reduction
met="rectangle" # choose the statistical summarizing measure ("max","sum","trapez",...)
RedMet="SRV" # reduction method ("SRV" or "bin")

pre_mQTL(aligdat, reducedF, RedMet="SRV",met, corrT=0.9)

}

\keyword{ methods }

