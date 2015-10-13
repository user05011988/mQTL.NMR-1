normalise_mQTL <-
function(infile,outfile,method,refIdx=1,noiseInt=c(11,12))
{ 

# infile [non-normalised file]
# outfile [normalised file]
# method: - Constant sum (method<-'CS')
#         - Qoutient probabilistic method (method<-'PQN')
#         - Linear baseline normalisation (method<-'LBN')
#         - Auto-scaling (method<-'AS')
#         - Pareto scaling (method<-'PS')
# L. Hedjazi, ICAN, 2014

if (nargs()<3)
{
    stop('incorrect number of input parameters')
}

data<-read.csv(infile,header=TRUE)
np=dim(data)[2]
X<-data[,-c(np-2,np-1,np)]
if (!is.matrix(X)){X<-as.matrix(X)}

  normD<-normalise(abs(X),method,refIdx,noiseInt)


data[,-c(np-2,np-1,np)]<-normD$Sp
write.table(data, outfile, quote=FALSE, row.names=FALSE,col.names=TRUE,sep=",")
}
