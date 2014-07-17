diff_res <-
function (res1, res2, nr=1000, nc=200, met=max){
# Function to compare 2 arrays by first binning them into a nr x nc matrix using the method met to summarize the region 

  dr1<-dim(res1)[1]
  dc1<-dim(res1)[2]
  dr2<-dim(res2)[1]
  dc2<-dim(res2)[2]
  if (nr>dr1 || nr>dr2){nr=min(dr1,dr2)}
  if (nc>dc1 || nc>dc2){nc=min(dc1,dc2)}
  res=matrix(0,nrow=nr,ncol=nc)

  print(paste("Setting up",nr,"x",nc,"from",dr1,"x",dc1,"and",dr2,"x",dc2))
  nr1<-c(1,floor(1:nr/nr*dr1))
  nc1<-c(1,floor(1:nc/nc*dc1))
  nr2<-c(1,floor(1:nr/nr*dr2))
  nc2<-c(1,floor(1:nc/nc*dc2))
  for(i in 1:nr){
    for( j in 1:nc){
      res[i,j]<-met(res1[nr1[i]:nr1[i+1],nc1[j]:nc1[j+1]])-met(res2[nr2[i]:nr2[i+1],nc2[j]:nc2[j+1]])
    }
  }

  return(res)
}
