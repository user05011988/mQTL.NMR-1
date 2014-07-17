selectRefSp <-
function(X,step)
{
# Automated selection of a reference spectrum based on the highest similarity to
# all other spectra
# Input: X - spectra
#        step - used to scale spectral regions down to specific bin_size

p<-numeric()
ref<-numeric()

obs<-dim(X)[1]
splength<-dim(X)[2]

if (step >= splength)
{
    CC<-cor(p,ref)
    CC<-CC[1,2]
    return()

}

bin_count<-ceiling(splength/step)
bin_width<-ceiling(splength/bin_count)
bins<-seq(1,splength, bin_width)

if (bins[length(bins)]!=splength) {
    bins<-c(bins,splength)
    bin_count<-bin_count+1
}

for (i in 1:(length(bins)-1))
{
    istart<-bins[i]
    iend<-bins[i+1]-1
    seglength<-iend-istart+1
    X[,istart:iend]<-X[,istart:iend]-apply(t(X[,istart:iend]),2,mean)*rep(1,seglength)
    stdX<-apply(t(X[,istart:iend]),1,sd)
    stdX[stdX==0]<-1
    X[,istart:iend]<-X[,istart:iend]/(stdX*rep(1,seglength))
}

CC<-abs(cor(t(X)))
index<-which(apply(CC,2,prod)==max(apply(CC,2,prod)))
return(index[1])
}
