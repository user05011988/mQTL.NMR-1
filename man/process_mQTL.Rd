\name{process_mQTL}
\alias{process_mQTL}
\title{
mQTL mapping 
}
\description{
Function to process the tissue extract of the individuals for QTL analysis
}
\usage{
process_mQTL(datfile, genfile, nperm = 0)
}
\arguments{
  \item{datfile}{
a text file with phenotype data
}
  \item{genfile}{
a text file with genotype data
}
  \item{nperm}{
\code{nperm} 
}
}
\details{
This function makes use of metabolomic and genotype data to perform QTL analysis based on the R/QTL package, for mapping quantitative trait loci. In particular, it makes use of the extended Haley-Knott method to optimize the LOD score evaluation and avoid problems with missing genotypes.
}
\value{
2D LOD score table 
}
\references{
Broman,K., et al (2006) R/qtl: QTL mapping in experimental crosses, Bioinformatics,
19(7), 889-890.
}
\author{
Jean-Baptiste Cazier and Hedjazi Lyamine
}

\seealso{
\code{\link{post_mQTL}}
}

\examples{

# Download data files

load_datafiles()

# mQTL mapping
results<- list() # a list to stock the mQTL mapping results
nperm<- 0 # number of permutations if required
results<-process_mQTL(reducedF, cleangen, nperm)

}

\keyword{ methods }

