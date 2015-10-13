SRV <-
function(X, minsize, correl, clustf=median){
  # Statistical Recoupling of Variables.
  # input: data matrix X, singlet size, bucketting resolution, correlation threshold
  # output: supercluster borders: indicesdebf and indicesfinf; 
  # superclusters.

  dX<-dim(X);
  X[is.nan(X)]<-0
  dim(X)<-dX
  Xct<-Centered(X)
  Xuv<-UVscaled(X)
  nr<-dim(X)[[1]]
  nc<-dim(X)[[2]]
  B<-rep(0,nc-1)


  print(paste("Processing a",nr,"x",nc,"matrix with minsize of",minsize," and a correlation Threshold of",correl,"using the",clustf))

  # Calculation of the covariance/correlation profile between consecutive variables
  for (j in 1:(nc-1)){
    B[j]<-var(Xct[,j],Xct[,j+1])/abs(cor(Xuv[,j],Xuv[,j+1]));
  }

  #Identification of the residual water area
  wat<-which(apply(X,2,sum)==0)
  if (length(wat)>0){
    lsup<-min(wat)-1
    linf<-max(wat)

    B[lsup:linf]<-0;
  }

  #Scan of the profile for the identification of local minima
  #starting and ending variables of each cluster are stored in indicesdeb and
  #indicesfin according to the covariance/correlation profile.
  #the first variable belongs to the first cluster and the last variabel to
  #the last cluster.  
  indicesdeb<-rep(0,nc);
  indicesfin<-rep(0,nc);
  ndeb<-1;
  nfin<-0;

  indicesdeb[ndeb]<-1;
  for (j in 2:(nc-2)){
    if ((B[j]<B[j-1]) && (B[j]<B[j+1])){
      nfin<-nfin+1;
      indicesfin[nfin]<-j;
      ndeb<-ndeb+1;
      indicesdeb[ndeb]<-j+1;
    }
  }
#  length(indicesdeb)<-ndeb
#  length(indicesfin)<-nfin

  if(indicesfin[nfin]!=nc){
  nfin=nfin+1
    indicesfin[nfin]<-nc;
  }
  length(indicesdeb)<-ndeb
  length(indicesfin)<-nfin

  #Measurement of the size of each cluster.
  #Comparison of the size with a resolution criterium:
  #floor(singletsize/resolution), where singletsize is the size of a resolved singlet in a 700MHz spectrum.
  #Clusters that do not contain at least a number of variables equals to "limit" are discarded
  indices<-indicesfin-indicesdeb+1; 
  si<-length(indices);
  destruction<-rep(0,si);
  ndestruction<-0;
  limit<-minsize
  for (j in 1:si){
    if(indices[j]<limit){
      ndestruction<-ndestruction+1;
      destruction[ndestruction]<-j;
    }
  }
  length(destruction)<-ndestruction
  if (ndestruction>0){
    indicesdeb=indicesdeb[-destruction];
    indicesfin=indicesfin[-destruction];
  }


  #Measurement of the mean value of the signal in each cluster stored in Xcluster.

  s1<-length(indicesdeb)
  print(paste("length of events:",s1))
  Xcluster<-matrix(0,nrow=nr,ncol=s1);
  jbstore<-matrix(0,nrow=nr*s1,ncol=4)
  colnames(jbstore)<-c("Max","Sum","Mean","Median")

  for (j in 1:s1){
    Xcluster[,j]<-apply(X[,indicesdeb[j]:indicesfin[j]],1,clustf)
    ind<-(j-1)*nr+1
    jbstore[ind:(ind+nr-1),1]<-as.vector(apply(X[,indicesdeb[j]:indicesfin[j]],1,max))
    jbstore[ind:(ind+nr-1),2]<-as.vector(apply(X[,indicesdeb[j]:indicesfin[j]],1,sum))
    jbstore[ind:(ind+nr-1),3]<-as.vector(apply(X[,indicesdeb[j]:indicesfin[j]],1,mean))
    jbstore[ind:(ind+nr-1),4]<-as.vector(apply(X[,indicesdeb[j]:indicesfin[j]],1,median))
  }

  print(c("Xcluster",dim(Xcluster),s1))

  #Correlation of neighboring clusters stored in Clustercorrelation.
  #Identification of highly correlated clusters.
  #We limit the agregation of clusters to 3 clusters according to a
  #sufficient level of correlation (0.9) that allow the identification of
  #chemically relevant superclusters (singlet, doublet, triplet, mulatiplet) on a 1D spectrum.
  Clustercorrelation<-rep(0,s1-1); 
  for (j in 1:(s1-1)){
    Clustercorrelation[j]<-abs(cor(c(Xcluster[,j],0.123456789),c(Xcluster[,j+1],0.1234566789)));
      # Add a hopefully unlikely constant to both vector to avoid a constant vector which result in NA correlation
  }
  
  n2<-0;
  balises<-rep(0,s1);
  for (j in 1:(s1-1)){
    if(Clustercorrelation[j]>correl){
      n2<-n2+1;
      balises[n2]<-j;
    }
  }
  length(balises)<-n2

  print(c("balise length:",n2))
  
  s2<-length(balises)
  choixbalises<-rep(0,s2);
  if(s2>1){ # If there are more than 1 supercluster
    for (k in 2:(s2-1)){
      if((balises[k]+1)==(balises[k+1])){
        choixbalises[k]<-1;
      }else{
#        if((balises[k]+1)!=(balises[k+1])){
          choixbalises[k]<-0;
#        }
      }
    }
    if (balises[1]+1== balises[2]){
      choixbalises[1]<-1;
    }else{ 
  choixbalises[1]<-0;
    }

    if (balises[s2]-1== balises[s2-1]){
      choixbalises[s2]<-1;
    }else{ 
    choixbalises[s2]<-0;
    }

    for (j in 2:(s2-1)){
      if ((choixbalises[j-1]==choixbalises[j]) &&(choixbalises[j+1]==choixbalises[j])){
        choixbalises[j]<-0;
      }
    }

    debcluster2<-rep(0,s2);
    fincluster2<-rep(0,s2);
    ndeb2<-0;
    nfin2<-0;
    for (k in 2:(s2-1)){
      if (choixbalises[k]==0 && choixbalises[k-1]!=1){
        ndeb2<-ndeb2+1;
        nfin2<-nfin2+1;
        debcluster2[ndeb2]<-balises[k];
        fincluster2[nfin2]<-balises[k];
      }else{
        if (choixbalises[k]>choixbalises[k-1]){
          ndeb2<-ndeb2+1;
          debcluster2[ndeb2]<-balises[k];
        }else{  
          if (choixbalises[k]==0 && choixbalises[k-1]==1){
            nfin2<-nfin2+1;
            fincluster2[nfin2]<-balises[k];
          }
        }
      } 
    }
    length(debcluster2)<-ndeb2
    length(fincluster2)<-nfin2

    if(choixbalises[s2-1]==1){
      nfin2<-nfin2+1;
      fincluster2[nfin2]<-balises[s2];
    }else{
      nfin2<-nfin2+1;
      ndeb2<-ndeb2+1;
      fincluster2[nfin2]<-balises[s2];
      debcluster2[ndeb2]<-balises[s2];
    }

    if(choixbalises[1]==1){
      debcluster2<-c(balises[1],debcluster2);
    }else{
      debcluster2<-c(balises[1],debcluster2);
      fincluster2<-c(balises[1],fincluster2);
    }

    #Measurement of the mean value of the clusters involved in a supercluster,
    #stored in Xcluster2.
    s3<-length(debcluster2);
    Xcluster2<-matrix(0,nrow=nr,ncol=s3);
    jbstore2<-matrix(0,nrow=nr*s3,ncol=4)
    colnames(jbstore2)<-c("Max","Sum","Mean","Median")
    for (j in 1:s3){
      Xcluster2[,j]<-apply(Xcluster[,debcluster2[j]:(fincluster2[j]+1)],1,clustf)
      ind<-(j-1)*nr+1
      jbstore2[ind:(ind+nr-1),1]<-as.vector(apply(Xcluster[,debcluster2[j]:(fincluster2[j]+1)],1,max))
      jbstore2[ind:(ind+nr-1),2]<-as.vector(apply(Xcluster[,debcluster2[j]:(fincluster2[j]+1)],1,sum))
      jbstore2[ind:(ind+nr-1),3]<-as.vector(apply(Xcluster[,debcluster2[j]:(fincluster2[j]+1)],1,mean))
      jbstore2[ind:(ind+nr-1),4]<-as.vector(apply(Xcluster[,debcluster2[j]:(fincluster2[j]+1)],1,median))
    }
  }else{ #If there is only 1 correlation
    if(s2==1){
      debcluster2<-balises[1]
      fincluster2<-balises[1]
      s3<-1
      Xcluster2<-matrix(0,nrow=nr,ncol=s3);
      jbstore2<-matrix(0,nrow=nr*s3,ncol=4)
      colnames(jbstore2)<-c("Max","Sum","Mean","Median")
      Xcluster2[,1]<-Xcluster[,debcluster2[1]]
      ind<-1
      print("jbstore2 single")
      jbstore2[,1]<-Xcluster[,debcluster2[1]]
      jbstore2[,2]<-Xcluster[,debcluster2[1]]
      jbstore2[,3]<-Xcluster[,debcluster2[1]]
      jbstore2[,4]<-Xcluster[,debcluster2[1]]
    }
  }

  #Clusters and superclusters are finally stored in Xclusterf, after 
  #adjustment of the cluster borders in case of overlapping after agregation.
  #Xclusterf<-matrix(0,nrow=nr,ncol=s3);
  Xclusterf<-matrix(0,nrow=nr,ncol=0);
  indicesdebf<-NULL;
  indicesfinf<-NULL;
  if (s2>1){ # If there are more than one superclusters Xcluster2
    if (debcluster2[1]>1){
      Xclusterf<-Xcluster[,1:(debcluster2[1]-1)];
      indicesdebf<-indicesdeb[1:(debcluster2[1]-1)];
      indicesfinf<-indicesfin[1:(debcluster2[1]-1)];
    }
    for (j in 1:(s3-1)){
      if (fincluster2[j]+2<=debcluster2[j+1]-1){
        Xclusterf<-cbind(Xclusterf,Xcluster2[,j],Xcluster[,(fincluster2[j]+2):(debcluster2[j+1]-1)]);
        indicesdebf<-c(indicesdebf,indicesdeb[debcluster2[j]],indicesdeb[(fincluster2[j]+2):(debcluster2[j+1]-1)]);
        indicesfinf<-c(indicesfinf,indicesfin[fincluster2[j]+1],indicesfin[(fincluster2[j]+2):(debcluster2[j+1]-1)]);
      }else{
        Xclusterf<-cbind(Xclusterf,Xcluster2[,j]);
        indicesdebf<-c(indicesdebf,indicesdeb[debcluster2[j]]);
        indicesfinf<-c(indicesfinf,indicesfin[fincluster2[j]+1]);
      }
    }
  
    if (fincluster2[s3]+2<=s1){
      Xclusterf<-cbind(Xclusterf,Xcluster2[,s3], Xcluster[,(fincluster2[s3]+2):s1]);
      indicesdebf<-c(indicesdebf,indicesdeb[debcluster2[s3]],indicesdeb[(fincluster2[s3]+2):s1]);
      indicesfinf<-c(indicesfinf,indicesfin[fincluster2[s3]+1],indicesfin[(fincluster2[s3]+2):s1]);
    }else{
      Xclusterf<-cbind(Xclusterf,Xcluster2[,s3])
      indicesdebf<-c(indicesdebf,indicesdeb[debcluster2[s3]]);
      indicesfinf<-c(indicesfinf,indicesfin[fincluster2[s3]+1]);
    }
  }else{
     if (s2==1){ # If there one supercluster Xcluster2
  #      print(paste("s1,s2,s3",s1,s2,s3))
  #      print("debcluster2, fincluster2")
  #      print(c(debcluster2, fincluster2))
  #      print("indicesdeb,indicesfin")
  #      print(c(indicesdeb,indicesfin))
        if (debcluster2[1]>1){
          Xclusterf<-Xcluster[,1:(debcluster2[1]-1)];
          indicesdebf<-indicesdeb[1:(debcluster2[1]-1)];
          indicesfinf<-indicesfin[1:(debcluster2[1]-1)];
        }
        Xclusterf<-cbind(Xclusterf,Xcluster2[,1]);
        indicesdebf<-c(indicesdebf,indicesdeb[debcluster2[1]]);
        indicesfinf<-c(indicesfinf,indicesfin[fincluster2[1]+1]);
        if (fincluster2[1]+2<=s1){
          Xclusterf<-cbind(Xclusterf,Xcluster[,(fincluster2[1]+2):s1]);
          indicesdebf<-c(indicesdebf,indicesdeb[(fincluster2[1]+2):s1]);
          indicesfinf<-c(indicesfinf,indicesfin[(fincluster2[1]+2):s1]);
        }
     }else{ # If there are no supercluser Xcluster2
       Xclusterf=Xcluster
       indicesdebf<-indicesdeb;
       indicesfinf<-indicesfin;
    }
  }

  nrt<-dim(Xclusterf)[[1]];
  nct<-dim(Xclusterf)[[2]];

  for (j in 1:(nct-1)){
    if (indicesdebf[j+1]<indicesfinf[j]){
      indicesfinf[j]<-indicesdebf[j+1]-1;
    }
  }

  print(paste("Found",length(indicesdebf)," clusters"))
  return(list(indicesdebf,indicesfinf, Xclusterf,jbstore,jbstore2,indicesdeb,indicesfin))
 # return(list(indicesdebf,indicesfinf,Xcluster,Xcluster2, Xclusterf,jbstore,jbstore2,indicesdeb,indicesfin))

}
