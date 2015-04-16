\name{normalise}
\alias{normalise}
\title{
Base function of normalisation
}
\description{
Removing dilutions between biofluid samples (normalisation of spectra)
}
\usage{
normalise(X, method,refIdx,noiseInt)
}
\arguments{
  \item{X}{
A matrix specifying metabolomic data
}
  \item{method}{
A character defining the normalization method. Constant sum normalisation (method<-'CS'), Constant noise normalisation (method<-'CN'), Quotient probabilistic method (method<-'PQN'), Linear baseline normalisation (method<-'LBN'), Auto scaling (method<-'AS'), Pareto scaling (method<-'PS').
}
  \item{refIdx}{
index of reference individual (set by the user if necessary)
}
  \item{noiseInt}{
noise region on the resonance axis as an interval (ex. [11,12] ppm) 
}
}
\value{
A matrix defining normalised spectrum
}
\references{
- Probabilistic quotient normalisation:
Dieterle, F., Ross, A., Schlotterbeck, G., & Senn, H. (2006). Probabilistic quotient normalization as robust method to account for dillution of complex biological mixtures. Application to 1H NMR metabolomics. Analytical Chemistry, 78, 4281-4290.

- Constant sum (total area) normalisation:
Craig, A., Cloarec, O., Holmes, E., Nicholson, J. K., Lindon, J. C., Scaling and normalization effects
in NMR spectroscopic metabonomic data sets. Anal Chem 2006, 78, (7), 2262-2267.

- Linear baseline normalisation:
Bolstad, B. M., Irizarry, R. A., Astrand, M., & Speed, T. P. (2003).
A comparison of normalization methods for high density oligonucleotide array data based on variance and bias. Bioinformatics,19, 185-193.

- Auto-scaling:
Jackson, J. E. (2003). A user's guide to principal components. Hoboken, NJ: Wiley-Interscience.

- Pareto scaling:
Eriksson, L., Antti, H., Gottfries, J., Holmes, E., Johansson, E., Lindgren, F., et al. (2004). Using chemometrics for navigating in the large data sets of genomics, proteomics, and metabonomics (gpm). Analytical and Bioanalytical Chemistry, 380, 419-429.

}

\author{
Lyamine Hedjazi
}
\seealso{
\code{\link{normalise_mQTL}}
}

\examples{

## Data
Sp=matrix(rnorm(10*5000,mean=0,sd=1), nrow=10,ncol=5000)

## Quotient probabilistic normalisation
NormDat<-normalise(abs(Sp),'PQN')

}

\keyword{ methods }
