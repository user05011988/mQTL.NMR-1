align_mQTL <-
function(datafile,outdat,idx)
{
# Recursive Segment-Wise Peak Alignment (RSPA) for accounting peak position
# variation across multiple 1H NMR biological spectra
# Output: Aligned spectra
# Author, L. Hedjazi, ICAN, Paris 2013

data<-read.csv(datafile,header=TRUE)
np<-dim(data)[2]
Sp<-data[,-c(np-2,np-1,np)]
if (!is.matrix(Sp)){Sp<-as.matrix(Sp)}

#ppm<-as.numeric(gsub("ppm_","",colnames(Sp)))

ppm<-as.numeric(sub("e\\.","e-",sub("ppm_","",sub("ppm_\\.","-", colnames(Sp)))))

recursion<-list()
peakParam<-list()
MAX_DIST_FACTOR<-numeric()
MIN_RC<-numeric()



setupRSPA(ppm)



obs<-dim(Sp)[1]
dim<-dim(Sp)[2]

if (nargs()>2)
{
refSp<-Sp[idx,]
}else{
print('...Automatic selection of a reference spectrum...')
idx<-selectRefSp(Sp,recursion$step)
refSp<-Sp[idx,]
}


#segmentate a reference spectrum
refSegments<- segmentateSp(refSp, peakParam)

for (index in 1:obs)
{
    #cat(" ",index)
    # segmentate a test spectrum 
    testSegments<-segmentateSp(Sp[index,], peakParam)

    # match test and reference segments
    attachedSegs<-attachSegments(refSegments,testSegments)

    refSegments<-attachedSegs$refSegmentsNew
    testSegments<-attachedSegs$testSegmentsNew

    Segs<-matchSegments(refSp,Sp[index,], testSegments,refSegments,MAX_DIST_FACTOR, MIN_RC)

    # align a test spectrum 

Sp[index,]<- alignSp(refSp,Segs$refSegs,Sp[index,],Segs$testSegs,recursion,MAX_DIST_FACTOR, MIN_RC)
 }
    print('')
    print('...Returning aligned spectra...')


data[,-c(np-2,np-1,np)]<-Sp
write.table(data, outdat, quote=FALSE, row.names=FALSE,col.names=TRUE,sep=",")

}
