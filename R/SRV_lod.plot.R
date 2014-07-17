SRV_lod.plot <-
function(results,file,Th) 
{

# Plot SRV clusters

for(i in 1:length(results)){assign(names(results)[i],results[[i]])}

summa <- summarize(results, file, Th)
max_idx<-which.max(summa[,6])
marker_idx<-which(summa[,4]==summa[max_idx,4] & summa[,5]==summa[max_idx,5])

M_SRV<- summa[marker_idx,1:3]

ppmS<- summa[marker_idx,3]
lodS<- summa[marker_idx,6]


plot(ppmS,lodS,lty=1,main=paste("LOD for locus",results[[5]],"on all SRV clusters"),lwd=2,ty="b",col=1,ylab="LOD",xlab="Resonance, ppm",pch=3,xlim=c(max(ppmS), min(ppmS)))
rug(ppmS,ticksize=-0.03,quiet=TRUE)

for (j in 1:length(ppmS)) {
ppmL<-summa[marker_idx,1][j]
ppmH<-summa[marker_idx,2][j]
  abline(v=ppmL,col=2); 
  abline(v=ppmH,col=3);
   }

}
