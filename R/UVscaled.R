UVscaled <-
function(A){
  #UVscaled Centre et reduit les variables d'une matrice.
  #   Class support for input A:
  #      float: double, single

  no<-dim(A)[[1]] 
  nv<-dim(A)[[2]] 
  s <-  sum(A);
  m <- s/no;
  Ua<-matrix(0,nrow=no,ncol=nv);
  m<-rep(0,nv)
  ET<-rep(0,nv)
  for (j in 1:nv){
    ET[j]<-sd(A[,j]);
  }
 
  for (j in 1:nv){
    M<-m[j]*rep(1,no);
    Ua[,j]<-(A[,j]-M)/ET[j];
  }

  return(Ua)
}
