load_demo_data <-
function(){

url1 <-"http://sourceforge.net/projects/mqtl/files/MetaboFile.txt/download"
url2<-"http://sourceforge.net/projects/mqtl/files/GenoFile.txt/download"
url3<-"http://sourceforge.net/projects/mqtl/files/physiodat.txt/download"
url4<-"http://sourceforge.net/projects/mqtl/files/met.clean.txt/download"
url5<-"http://sourceforge.net/projects/mqtl/files/gen.clean.txt/download"
url6<-"http://sourceforge.net/projects/mqtl/files/rectangle_SRV.txt/download"
url7<-"http://sourceforge.net/projects/mqtl/files/ur.alig.txt/download"
url8<-"http://sourceforge.net/projects/mqtl/files/cs.norm.txt/download"
url9<-"http://sourceforge.net/projects/mqtl/files/pqn.norm.txt/download"
url10<-"http://sourceforge.net/projects/mqtl/files/ur.rectangle.alig.txt/download"

phenofile<-"MetaboFile.txt"
download.file(url1,phenofile,mode="w")

genofile<-"GenoFile.txt"
download.file(url2,genofile,mode="w")

phyfile<-"physiodat.txt"
download.file(url3,phyfile,mode="w")

cleandat<-"met.clean.txt"
download.file(url4,cleandat,mode="w")

cleangen<-"gen.clean.txt"
download.file(url5,cleangen,mode="w")

rectnagle_SRV<-"rectangle_SRV.txt"
download.file(url6,rectnagle_SRV,mode="w")

aligD<-"ur.alig.txt"
download.file(url7,aligD,mode="w")

CSnorm<-"cs.norm.txt"
download.file(url8,CSnorm,mode="w")

PQNnorm<-"pqn.norm.txt"
download.file(url9,PQNnorm,mode="w")

reducedF<-"reducedF.txt"
download.file(url10,reducedF,mode="w")

assign("phenofile", phenofile, envir= parent.frame())
assign("genofile", genofile, envir= parent.frame())
assign("physiodat",phyfile , envir= parent.frame())
assign("cleandat", cleandat, envir= parent.frame())
assign("cleangen", cleangen, envir= parent.frame())
assign("rectnagle_SRV", rectnagle_SRV, envir= parent.frame())
assign("aligdat", aligD, envir= parent.frame())
assign("CSnorm", CSnorm, envir= parent.frame())
assign("PQNnorm", PQNnorm, envir= parent.frame())
assign("reducedF", reducedF, envir= parent.frame())
}
