set_baseline <-
function(nr,nc,b=1){
# Script to create a simulated baseline
  print(c(nr,nc))
  out<-runif(nr*nc,min=0,max=b)
  print(length(out))
  print(dim(out))
  dim(out)<-c(nr,nc)
  print(dim(out))
  return(out)
}
