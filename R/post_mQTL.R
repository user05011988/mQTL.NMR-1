post_mQTL <-
function(results, probs=c(0.95,0.99,0.999,0.9999)){
# Function to plot the results of a given run
# Input:  2D results of LOD scores
# Output: Graphs and summaries
 
    print(paste("Post processing"))
    for(i in 1:length(results)){assign(names(results)[i],results[[i]])}

    ## PLOT THE RESULTS
    
    dev.new(width=11,height=11,pointsize=10)
    split.screen(c(2,2))
    split.screen(c(2,1),screen=4)
    split.screen(c(2,1),screen=1)
    
    title<-paste("for all using:\n",results$maxiL,"for",results$ppm[results$top],"ppm at",results$maxi)
    screen(7)
    qt<-quantile(results$res,probs=probs,na.rm=TRUE)
    h<-hist(results$res,br=50,freq=FALSE,main="LOD Distribution",xlab="LOD")
    abline(v=qt,col=c(1,4,3,2),lt=3)


    screen(8)
    plot(results$ppm,results$res[which(results$res==max(results$res),arr.ind=TRUE)[1] ,],lty=1,main=paste("LOD for top locus",results$maxi),ty="b",col="red",ylab="LOD",xlab="Resonance, ppm",pch=3,xlim=c(max(results$ppm), min(results$ppm)))
    rug(results$ppm,ticksize=-0.03,quiet=TRUE)
    
    screen(6)
    plot(results$best,lty=1,main=paste("LOD for top Shift",results$ppm[results$top],"ppm"))
    
    screen(5)
    topc<-summary(results$best)[which.max(summary(results$best)[,3]),1]
    plot(results$best,lty=1,main=paste("LOD for top Shift",results$ppm[results$top],"ppm on chr",topc),chr=topc,col="blue")
    
    screen(3)
    pplot(results$res,"Full 2D Profile", results$ppm, results$best, quantile(results$res,probs=probs,na.rm=TRUE))
    
    screen(2)
    ppersp(results$res, results$ppm, paste("Top LOD",title))
    
    close.screen(all.screens=TRUE)

## save in separate plots

    png("Lod distribution.png")
    qt<-quantile(results$res,probs=probs,na.rm=TRUE)
    h<-hist(results$res,br=50,freq=FALSE,main="LOD Distribution",xlab="LOD")
    abline(v=qt,col=c(1,4,3,2),lt=3)
    dev.off()
    

    png("Lod for top locus.png")
    plot(results$ppm,results$res[which(results$res==max(results$res),arr.ind=TRUE)[1] ,],lty=1,main=paste("LOD for top locus",results$maxi),ty="b",col="red",ylab="LOD",xlab="Resonance, ppm",pch=3,xlim=c(max(results$ppm), min(results$ppm)))
    rug(results$ppm,ticksize=-0.03,quiet=TRUE)
    dev.off()


    png("Lod for top shift.png")
    plot(results$best,lty=1,main=paste("LOD for top Shift",results$ppm[results$top],"ppm"))
    dev.off()


    png("Lod for top shift2.png")
    topc<-summary(results$best)[which.max(summary(results$best)[,3]),1]
    plot(results$best,lty=1,main=paste("LOD for top Shift",results$ppm[results$top],"ppm on chr",topc),chr=topc,col="blue")
    dev.off()


    png("Full 2D Profile.png")
    pplot(results$res,"Full 2D Profile", results$ppm, results$best, quantile(results$res,probs=probs,na.rm=TRUE))
    dev.off()
 

    png("3D Profile.png")
    ppersp(results$res, results$ppm, paste("Top LOD",title))
    dev.off()
}
