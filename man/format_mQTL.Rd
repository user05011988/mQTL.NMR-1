\name{format_mQTL}
\alias{format_mQTL}
\title{
Routine to reformat the data of animal crosses into the required format to perform mQTL mapping
}
\description{
This function enables to reformat data into the proper format. The user should provides in input metabolomic file, Genotype file and a file containing sex and pgm (parental grandmother). 
}
\usage{
format_mQTL(datafile, genofile, physdat, outdat, outgeno)
}

\arguments{
  \item{datafile}{
   metabolomic data file in text format
}
  \item{genofile}{
   genotype data file in text format
}
  \item{physdat}{
   a file containing sex and pgm in text format 
}
  \item{outdat}{
   Output data file with formatted phenotype data (metabolomic data + sex + pgm) in the format csvs
}
  \item{outgeno}{
   Output data file with formatted genotype data in the csvs format
}
}

\value{
formatted phenotype and genotype data files (in format csvs) are written to the user working directory (it is therefore preferable that the user create a new directory to be used throughout the study) 
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{align_mQTL}},
}

\examples{

# Download data files
load_datafiles()

# Format data

format_mQTL(phenofile,genofile,physiodat,cleandat,cleangen)

}

\keyword{ function }

