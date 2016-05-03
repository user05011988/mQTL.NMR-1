\name{Top_SRV.plot}
\alias{Top_SRV.plot}
\title{
Plot top SRV clusters
}
\description{
Plot lines defined by SRV on top SRV clusters
}
\usage{
Top_SRV.plot(file1,file2,results,met,intMeth,clustidx)
}

\arguments{
  \item{file1}{
a text file with NMR data
}
  \item{file2}{
a text file with SRV clusters
}
  \item{results}{
a list containinig results of mQTL mapping 
}
  \item{met}{
a character specifying the summarizing statistical measure of peaks
}
  \item{intMeth}{
a charcter specifying summarizing method across samples ("mean" or "max")
}
  \item{clustidx}{
index specifying the SRV cluser of interest  (optinal)
}
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{SRV.plot}}
}
\examples{

load_datafiles()

load(results)

# Format data

format_mQTL(phenofile,genofile,physiodat,cleandat,cleangen)

## Plot SRV profile
Top_SRV.plot(file1=cleandat,file2=rectangle_SRV,results=results,met=met,intMeth="mean")

}

\keyword{ function }
