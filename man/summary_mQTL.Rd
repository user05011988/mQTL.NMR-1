\name{summary_mQTL}
\alias{summary_mQTL}
\title{
Function to summarize the mQTL mapping results of all the runs and their differences}
\description{
This function generates a table containing the genetic markers and thier associated metabolomic variables and estimated LOD score.
}
\usage{
summary_mQTL(results, redfile,Th = 5)
}
\arguments{
  \item{results}{
a list sepcifying the mQTL mapping results
}
  \item{redfile}{
a text file containing the parameters of identified clusters(.PPM file)}
  \item{Th}{
a numerical paramater indicating the threshold of top accepted score (LOD or -log10(p-value))  
}
}
\details{
Generates a text file containing a table of summary of mQTL mapping results 
}
\value{
returns Summaries
}
\author{
Jean-Baptiste Cazier and Lyamine Hedjazi
}

\seealso{
\code{\link{pre_mQTL}}
}

\examples{
    load_datafiles()
    load(results)

    Th<-10 ## LOD threshold
    summary_mQTL(results,rectangle_SRV,Th)## summarizes mQTL results in a table
}

\keyword{methods}

