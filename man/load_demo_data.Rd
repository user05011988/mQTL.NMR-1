\name{load_demo_data}
\alias{load_demo_data}

\title{
Load demo data files  
}

\description{
Data files are downloaded from the sourceforge.net website to the user's working directory.    
}

\usage{
load_demo_data()
}


\value{
Loaded data files concern four datasets: raw metabolomic data (Metabofile.txt), genomic data (Genofile.txt), additional data (physiodat.txt), formatted metabolomic data (met.clean.txt) and formatted genomic data (gen.clean.txt). Data files specifying additional information and results are also provided such as: result of SRV clustering (ur.rectangle.alig.txt), aligned data (ur.alig.txt), normalized data by CS and PQN methods (cs.norm.txt and pqn.norm.txt) and SRV clusters parameters (rectangle_SRV.txt)      
}

\author{
Lyamine Hedjazi
}

\seealso{
\code{\link{format_mQTL}}
}

\examples{

 \dontrun{

# Load demo data files
 load_demo_data()

}
}

\keyword{function}

