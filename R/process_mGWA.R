process_mGWA <-
function(phenofile=phenofile, genofile=genofile,nperm=0, gtmodel="overdominant", covarList=c("sex","age")){
# reformat phenofile
phenodat<-read.csv(phenofile,header=TRUE)
nph<-dim(phenodat)[2]
phenodat<-cbind(phenodat[,c(nph-2,nph-1,nph)],phenodat[,-c(nph-2,nph-1,nph)])
f<- basename(phenofile)
outpheno<-paste(sub("[.][^.]*$", "", f),"format.txt",sep="_")
write.table(phenodat,outpheno,quote=FALSE,row.names=FALSE,sep='\t')

# Generate the gwaa data
data<-load.gwaa.data(phenofile=outpheno,genofile=genofile, force=TRUE)

np<-nph-3
ppm<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(data@phdata)[4:nph]))))
k=1:np
TotM<-nsnps(data)
print(paste("TotM is ",TotM,"and np is",np))
res = matrix(nrow=TotM,ncol=np)
#resp = matrix(nrow=TotM,ncol=np)
#respc = matrix(nrow=TotM,ncol=np)
P1df = matrix(nrow=TotM,ncol=np)
#Pc1df = matrix(nrow=TotM,ncol=np)
permo = list()

for (i in 1:np){

    if (!missing(covarList)){
    fmla <- as.formula(paste("phdata(data)[,i+3]~", paste(covarList, collapse= "+")))
    }else{
    fmla<- as.formula(paste("phdata(data)[,i+3]~1"))
    }

    best1<- mlreg(formula=fmla, data, gtmode = gtmodel, trait.type = "gaussian")

    P1df[,i]<-best1[,"P1df"]
    #Pc1df[,i]<-best1[,"Pc1df"]
    #if(i%%(min(round(np/10),100)==0)){
      #print(c(i,max(P1df,na.rm=TRUE)))
     # summary(best1)
    #}
  }
  res[,k]=-log10(P1df)
  #resp[,k]=P1df
  #resc[,k]=-log10(Pc1df)
  #respc[,k]=Pc1df

  top<-ceiling(which.max(res)/TotM)
    
  best<-list()
  best$chr<- best1[,"Chromosome"]
  best$pos<- best1[,"Position"]
  best$lod<- res[,top]
  class(best)<-c("scanone","data.frame")
  rownames(best)<-snpnames(data)
  
  i=top
  best2<- mlreg(formula=fmla, data, gtmode = gtmodel, trait.type = "gaussian")

  maxi<-rownames(best)[which.max(res)-(top-1)*TotM]
  maxiL<-round(max(res,na.rm=TRUE),2)
  print(paste("SRV glm in this area was",maxiL,"for",ppm[top],"ppm at",maxi))
  if(nperm>0){
    summary(best,perms=permo[[top]],pvalues=TRUE)
  }else{
    summary(best,maxiL-1)
    descriptives.scan(best2)
  }

return(list(res=res,ppm=ppm,best=best,best2=best2,top=top,maxi=maxi,maxiL=maxiL,permo=permo))

}
