drawstraightmetabolome <-
function (ppm) 
{
avalues<-cbind((ppm-5)/5.4,rep(0,length(ppm)))
lines(avalues, cex = 0.01)
points(avalues,  pch = 20)
text(t(c(max(avalues[,1]+0.04), 0)),paste(max(ppm)),cex=0.6)
text(t(c(min(avalues[,1]-0.04), 0)),paste(min(ppm)),cex=0.6)
    }
