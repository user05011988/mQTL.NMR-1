\name{mQTL.NMR-package}
\alias{mQTL.NMR-package}
\alias{mQTL.NMR}
\docType{package}
\title{
Metabolomic Quantitative Trait Locus mapping for 1H NMR data 
}
\description{
mQTL.NMR provides a complete mQTL analysis pipeline for 1H NMR data. Distinctive features include normalisation using most-used approaches, peak alignment using RSPA approach, dimensionality reduction using SRV and binning approaches, and mQTL analysis for animal and human cohorts.
}
\details{
\tabular{ll}{
Package: \tab mQTL.NMR\cr
Type: \tab Package\cr
Version: \tab 0.99.2\cr
Link: \tab http://www.ican-institute.org/tools\cr
Date: \tab 2014-05-19\cr
License: \tab Artistic-2.0\cr
}
Main fucntions:
\itemize{
\item \code{format_mQTL}: generates the proper format of animal crosses data
\item \code{format_mGWA}: generates the proper format of human data
\item \code{align_mQTL}: peak alignment
\item \code{normalise_mQTL}: normalisation of metabolomic data using different approaches (Probabilistic quotient, constant sum,...)
\item \code{pre_mQTL}: dimension reduction by statistical recoupling of variables or bining
\item \code{process_mQTL}: computes LODs using extended Haley-Knott method for animal crosses
\item \code{process_mGWA}: computes p-values using a standard linear regression approach for human
\item \code{post_mQTL}: plots the results of a given run
\item \code{summary_mQTL}: provides the results as a table
\item \code{simple.plot}: Plots a region of NMR profile
\item \code{SRV.plot}: Plots the regions identified by SRV in NMR profiles
\item \code{ppersp}: Plot 3-D profile of LODs as function of genomic position and chemical shift
\item \code{pplot}: Plot a color scale layer
\item \code{Top_SRV.plot}: Plot top SRV clusters for structural assignment
\item \code{circle_mQTL}: Plot a circular genome-metabolome plot
}
}

\author{
Lyamine Hedjazi and Jean-Baptiste Cazier

Maintainer: Lyamine Hedjazi  <\email{mqtl@ican-institute.org}>
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

# mQTL mapping
results<- list() # a list to stock the mQTL mapping results
nperm<- 0 # number of permutations if required
results<-process_mQTL(reducedF, cleangen, nperm)

## Post-Process
post_mQTL(results)

## Summarize 
redfile<-"rectangle_SRV.ppm"
summary_mQTL(results,redfile,T=8)

#plot circular genome
circle_mQTL(results, Th=8,spacing=0)

## visualisation and metabolite identification
#plot NMR profile
simple.plot(file=cleandat,lo=3.02,hi=3.08,k=1:20,title="NMR profile") 

#plot SRV regions
SRV.plot(file1=cleandat,file2=rectangle_SRV,lo=3.02,hi=3.08,k=1:20,title="Cluster plot")

#plot lod for the region of interest
SRV_lod.plot(results,rectangle_SRV,Th=1)

#plot top lod SRV regions
Top_SRV.plot(file1=cleandat,file2=rectangle_SRV,results=results,met=met,intMeth="mean")

}
