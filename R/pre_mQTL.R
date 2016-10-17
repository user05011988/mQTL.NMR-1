pre_mQTL <-
function(infile, outfile, RedMet="SRV", met="sum",corrT=0.9,BinWidth=0.01){
  # Routine to preprocess the NMR file into SRV or bins 
 
  dat<-read.csv(infile)
  ind<-row.names(dat)
  n_ind<-length(ind)
  data<-matrix()
  np<-dim(dat)[[2]]-3
  dat2<-as.matrix(dat[,1:np])
  p<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(dat2)))))
  dat2[which(is.na(dat2))]=0

switch(RedMet,
SRV={
  if(met!="running"){

    #Perform the SRV analysis to reduce the number of dimension
    #meth<-get(met)
  
    SRV<-SRV(dat2, 10, corrT,clustf=met)
    data<-SRV[[3]]
    #data<-log10(SRV[[3]]+abs(floor(min(SRV[[3]]))))
    ppm<-(p[SRV[[1]] ]+p[SRV[[2]] ])/2
    nppm<-length(ppm)
    
    write.table(t(rbind(p[SRV[[1]] ] ,p[SRV[[2]] ],ppm)),paste(met,"SRV.ppm",sep="_"),col.names=c("Min","Max","Mean"),sep="\t",row.names=FALSE,quote=FALSE)
  
  }else{ # Use the original Running function rather than SRV
    data<-matrix(nrow=n_ind, ncol=np)
    mymid<-25
    mybase<-5
    for(r in 1:n_ind){
      data[r,] <- running(dat2[r,],width=0.00025,mid=mymid,base=mybase)
    }
    ppm<-p
    nppm<-length(ppm)
    # Drop the border effect
    start<-mymid+mybase+1
    end<-nppm-mymid-mybase-1
    
    data[,1:start]<-0 # Set borders to 0 
    data[,end:nppm]<-0
  }},
bin={

print(paste("Processing a",n_ind,"x",np,"matrix with a bind width of",BinWidth,"using the",met,sep=' '))
indicedeb<-vector()
indicefin<-vector()
SampInterval<- p[2]-p[1]

#for (i in 1:np-1)
#{
#while(ppm[i+1]<4.5 || ppm[i]>=6) {SampInterval[i]<- ppm[i+1]-ppm[i]}
#}

print(paste("sampling interval:",SampInterval,sep=' '))

cardN<-round(BinWidth/SampInterval)

cardB<-round(np/cardN)

print(paste("found",cardB,"bins",sep=' '))

k=1:cardB
indicedeb<-cardN*(k-1)+1
indicefin<-cardN*k
indicefin[cardB]<-np
indiceB<-cbind(indicedeb,indicefin)

data<-matrix(nrow=n_ind, ncol=cardB)

for(j in 1:cardB) {
data[,j]<-as.vector(apply(dat2[,indicedeb[j]:indicefin[j]], 1, met)) 
}

print(paste("reduced dataset:",n_ind,"x",cardB,sep=' '))

ppm <- (p[indicedeb] + p[indicefin])/2
nppm<-length(ppm)
write.table(t(rbind(p[indicedeb], p[indicefin], ppm)),paste(met,"bin.ppm",sep='_'), col.names = c("Min", 
"Max", "Mean"), sep = "\t", row.names = FALSE, quote = FALSE)

print(paste("list of bins: data file",paste(met,"bin.ppm",sep='_'),"generated",sep=' '))

})

  Dlog10_1 <- log10(data+abs(floor(min(data)))+1)

  dim(Dlog10_1)<- c(n_ind,nppm)
  
  #Drop outliers
  #drop_outliers(Dlog10_1,nppm,2)
  
  dat3<-cbind(Dlog10_1,dat[1:3+np])
  colnames(dat3)<-c(paste("ppm",ppm,sep="_"),colnames(dat)[c(np+1,np+2,np+3)])
  if(!exists("outfile")){outfile<-paste("ur",met,RedMet,"dat",sep=".")}
  write.csv(dat3,outfile,quote=FALSE,row.names=FALSE)
  print(paste("dataset: data file",outfile,"generated",sep=' '))

}
