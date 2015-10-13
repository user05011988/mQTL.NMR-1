\name{format_mGWA}
\alias{format_mGWA}
\title{
Routine to reformat the data into the required format to perform mGWAS
}
\description{
This function enables to reformat data into the proper format. The user should provides in input metabolomic file, Genotype file, map file and a file containing sex, age and individual IDs. 
}
\usage{
format_mGWA(datafile, genofile1, genofile2, covarfile, outdat, outgeno)
}

\arguments{
  \item{datafile}{
metabolomic data file
}
  \item{genofile1}{
genotype file in the "ped" format
}
  \item{genofile2}{
map file containing more information on SNP marker (position, ...)
}
  \item{covarfile}{
a text file contains covariates such as age or sex 
}
  \item{outdat}{
output data file with formatted phenotype data in csvs format
}
  \item{outgeno}{
output data file with formatted genotype data in csvs format
}
}

\value{
formatted phenotype and genotype data files (in format csvs) are written to the user working directory (it is therefore preferable that the user create a new directory to be used throughout the study)  
}

\author{
Lyamine Hedjazi
}

\seealso{
 \code{\link{format_mQTL}}, \code{\link{process_mGWA}}
}
\examples{
load_datafiles()
format_mGWA(human.pheno, human.geno, humanMap, covarFile,cleandat, cleangen)
}

\keyword{ function }

