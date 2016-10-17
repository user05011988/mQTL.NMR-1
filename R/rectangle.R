rectangle <-
function(data){

  n=length(data)
  s=sum(data)
  t=min(data)*n
  if(s-t<0){print(paste(s,t,n,s-t))}
  #s=s-(data[1]+data[n])*n/2
 # print(c(min(data),max(data),s,t))
  return(s-t)
}
