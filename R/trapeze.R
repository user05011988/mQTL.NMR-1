trapeze <-
function(data){
  trapeze <- 0
  n=length(data)
  s=sum(data)
  a=(data[n]-data[1])/n
  b=data[1]-a
  t=sum(pmin(data,1:n*a+b))
  #s=s-(data[1]+data[n])*n/2
 # print(c(min(data),max(data),s,t))
  return(s-t)
}
