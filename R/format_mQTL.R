format_mQTL <-
function(datafile, genofile, physdat,outdat, outgeno){
  # Routine to reformat the data into the required csvs used by the R/QTL package
  print(paste("Start formatting the datafile",datafile,"and the genotype file",genofile,"into the csvs files:",outdat,outgeno))

  ur.dat<-read.table(datafile,as.is=TRUE,header=TRUE,sep="\t")
  ur.geno<-read.table(genofile,as.is=TRUE,header=TRUE,na.strings="ND",sep="\t")
  ur.mrks<-colnames(ur.geno[,-1])
  ur.genmouse<-paste(ur.geno[-c(1,2,3),1],sep="")

  #ur.gen<-matrix(9,nrow=dim(ur.geno.CHD)[[1]]+dim(ur.geno.HFD)[[1]]-5,ncol=(dim(ur.geno.CHD)[[2]]+dim(ur.geno.HFD)[[2]]-1)
  ur.gen<-matrix("NA",nrow=dim(ur.geno)[[1]]-3,ncol=dim(ur.geno)[[2]]-1)
  ur.gen[ur.geno[-c(1,2,3),-1]=="a"]="A"
  ur.gen[ur.geno[-c(1,2,3),-1]=="h"]="H"
  ur.gen[ur.geno[-c(1,2,3),-1]=="b"]="B"
  dim(ur.gen)<-c(dim(ur.geno)[[1]]-3,dim(ur.geno)[[2]]-1)
  row.names(ur.gen)<-ur.genmouse
  colnames(ur.gen)<-ur.mrks
  # Define the set of samples to use with genotype, phenotype and data, in the genotype order
  ur.set<-intersect(ur.genmouse,colnames(ur.dat))
  #ur.dat.names<-grep("X",unlist(strsplit(ur.set,"_")),value=TRUE)
  ur.gen2<-ur.gen[ur.set,]
  #write.table(ur.gen2,file="ur_geno.ehk.dat",sep="\t",row.names=FALSE,col.names=FALSE)
  
  ur.data<-ur.dat[,ur.set]
  ur.nm<-dim(ur.data)[2]
  
  ## PREPARE THE DATA
  ur.chr<-ur.geno[1,-1]
  ur.loc<-ur.geno[2,-1]
  
  # Export the cleaned data in the proper csvs format
  ur.dat2<-t(ur.data)
  ur.p<-as.numeric(colnames(ur.dat2))


   ur.extr<-read.table(physdat,as.is=TRUE,header=TRUE,sep="\t")
   sex<-ur.extr[1,ur.set]
   pgm<-ur.extr[2,ur.set]

   ur.dat3<-cbind(ur.dat2, t(sex), t(pgm), rownames(ur.gen2))



  colnames(ur.dat3)<-c(paste("ppm",ur.p,sep="_"),"sex","pgm","id")
  write.csv(ur.dat3,outdat,quote=FALSE,row.names=FALSE)
  
  ur.gen3<-rbind(ur.chr, ur.loc, ur.gen2)
  ur.gen3<-cbind(c("","",rownames(ur.gen3[-c(1,2),])), ur.gen3)
  colnames(ur.gen3)[1]<-"id"
  write.csv(ur.gen3, outgeno, quote=FALSE, row.names=FALSE)
}
