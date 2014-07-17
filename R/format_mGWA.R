format_mGWA <-
function(datafile, genofile1, genofile2, covarfile,outdat, outgeno){
  # Routine to reformat the data into the required format used by the GenABEL package
  print(paste("Start formatting the datafile",datafile,"and the genotype files",genofile1,genofile2,"into files:",outdat,outgeno))

  ur.dat<-read.table(datafile,as.is=TRUE,header=TRUE,sep="\t")

  # Export the cleaned data in the proper csvs format
  ur.dat2<-t(ur.dat)
  ur.p<-as.numeric(colnames(ur.dat2))

  ur.covar<-read.table(covarfile,as.is=TRUE,header=TRUE,sep="\t")

  ur.dat3<-cbind(ur.dat2, ur.covar)

  colnames(ur.dat3)<-c(paste("ppm",ur.p,sep='_'),colnames(ur.covar))
  write.csv(ur.dat3,outdat,quote=FALSE,row.names=FALSE)

  
  # Format genotype data
  #f <- basename(genofile1)
  #ext<-sub("[.][^.]*$", "", f)
  
  convert.snp.ped(pedfile=genofile1, mapfile=genofile2,outfile=outgeno,strand="u")
}
