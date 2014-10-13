sgolay <-
function(k,F,W)
{
# Check if the input arguments are valid
if (round(F) != F) {stop('Frame length must be an integer.')}
if (F%%2!= 1) {stop('Frame length must be odd.')}
if (round(k)!= k) {stop('Polynomial degree must be an integer.')}
if (k > F-1) {stop('The degree must be less than the frame length.')}
if (nargs() < 3) {
   # No weighting matrix, make W an identity
   W <- diag(F)
}else{
   #Check for right length of W
   if (length(W) != F) { stop('The weight vector must be of the same length as the frame length.')}
   #Check to see if all elements are positive
   if (min(W) <= 0) {stop('All the elements of the weight vector must be greater than zero.')}
   #Diagonalize the vector to form the weighting matrix
   W <- diag(W)
}

#Compute the projection matrix B

S<-outer((-(F-1)/2):((F-1)/2), 0:k, FUN="^") # Compute the Vandermonde matrix  

qrm<- qr(sqrt(W)%*%S)

R<-upper.tri(qrm$qr,diag=TRUE)*qrm$qr

G<-S %*% pinv(R)%*% t(pinv(R))# Find the matrix of differentiators

B <- G%*%t(S)%*%W

return(G)
}
