\name{process_mGWA}
\alias{process_mGWA}

\title{
Metabolomic Genome-Wide Association analysis for a set of independent individuals
}
\description{
Test for association between a trait and genetic polymorphism
}
\usage{
process_mGWA(phenofile = phenofile, genofile = genofile, nperm = 0, gtmodel = "overdominant", covarList = c("sex", "age"))
}
\arguments{
  \item{phenofile}{
a text file with phenotype data
}
  \item{genofile}{
a text file with genotype data
}
  \item{nperm}{
number of permutations
}
  \item{gtmodel}{
genetic model ("additive","recessive","dominant","overdominant")
}
  \item{covarList}{
covariate variables ("sex" and/or "age")
}
}
\details{
This function makes use of metabolomic and genotype data to perform genome-wide association analysis using a standard regression method based on the GenABEL package.
}
\value{
2D score tables (-log10(p-value)) 
}
\references{
Aulchenko, Y.S.; Ripke, S.; Isaacs, A.; van Duijn, C.M. Bioinformatics 2007, 23, 1294-1296.
}
\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{format_mGWA}}
}
\examples{

load_datafiles()
format_mGWA(human.pheno, human.geno, humanMap, covarFile,hcleandat, hcleangen)

# mQTL mapping
results<- list() # a list to stock the mQTL mapping results
nperm<- 0 # number of permutations if required

results<-process_mGWA(phenofile=hreducedF, genofile=hcleangen,nperm=0, gtmodel="additive")
}

\keyword{ function }
