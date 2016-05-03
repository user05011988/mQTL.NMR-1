Centered <-
function(A){
  # Centered variables of a matrix.
  #   Class support for input A:
  #      float: double, single
  
  no<-dim(A)[[1]] 
  s <-  sum(A);
  m <- s/no;
  
  Ca<-A-m;   

  return(Ca)
}
