load_datafiles <-
function(){

# Mouse files
phenofile <- system.file("extdata", "phenofile.txt", package="mQTL.NMR",lib.loc=NULL)
genofile <- system.file("extdata", "genofile.txt", package="mQTL.NMR",lib.loc=NULL)
physiodat <- system.file("extdata", "physiodat.txt", package="mQTL.NMR",lib.loc=NULL)
results <- system.file("extdata", "results.rda", package="mQTL.NMR",lib.loc=NULL)
rectangle_SRV <- system.file("extdata", "rectangle_SRV.ppm", package="mQTL.NMR",lib.loc=NULL)
reducedF<-system.file("extdata", "reducedF.txt", package="mQTL.NMR",lib.loc=NULL)

cleandat<-"met.clean.txt"
cleangen<-"gen.clean.txt"
aligdat<-"aligdat.txt"
CSnorm<-"cs.norm.txt"
PQNnorm<-"pqn.norm.txt"


# Human files
human.pheno<- system.file("extdata", "human.pheno.txt", package="mQTL.NMR",lib.loc=NULL)
human.geno <- system.file("extdata", "human.geno.ped", package="mQTL.NMR",lib.loc=NULL)
humanMap <- system.file("extdata", "humanMap.map", package="mQTL.NMR",lib.loc=NULL)
covarFile <- system.file("extdata", "covarFile.txt", package="mQTL.NMR",lib.loc=NULL)
hreducedF<-system.file("extdata", "hreducedF.txt", package="mQTL.NMR",lib.loc=NULL)

hcleandat<-"hmet.clean.txt"
hcleangen<-"hgen.clean.txt"
haligdat<-"haligdat.txt"
hCSnorm<-"hcs.norm.txt"
hPQNnorm<-"hpqn.norm.txt"

assign("phenofile", phenofile, envir= parent.frame())
assign("genofile", genofile, envir= parent.frame())
assign("physiodat",physiodat , envir= parent.frame())
assign("results", results, envir= parent.frame())
assign("human.pheno", human.pheno, envir= parent.frame())
assign("human.geno", human.geno, envir= parent.frame())
assign("humanMap", humanMap, envir= parent.frame())
assign("covarFile", covarFile, envir= parent.frame())
assign("cleandat", cleandat, envir= parent.frame())
assign("cleangen", cleangen, envir= parent.frame())
assign("rectangle_SRV", rectangle_SRV, envir= parent.frame())
assign("aligdat", aligdat, envir= parent.frame())
assign("CSnorm", CSnorm, envir= parent.frame())
assign("PQNnorm", PQNnorm, envir= parent.frame())
assign("reducedF", reducedF, envir= parent.frame())
assign("hcleandat", hcleandat, envir= parent.frame())
assign("hcleangen", hcleangen, envir= parent.frame())
assign("haligdat", haligdat, envir= parent.frame())
assign("hCSnorm", hCSnorm, envir= parent.frame())
assign("hPQNnorm", hPQNnorm, envir= parent.frame())
assign("hreducedF", hreducedF, envir= parent.frame())
}
