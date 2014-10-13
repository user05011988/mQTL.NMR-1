circle_mQTL <-
function (results, Th=0, chr=9, spacing = 25) 
{
       
        templateresult<-results$best
        ppmt<-results$ppm  
        signif_ppms<-as.data.frame(which(results$res>Th,arr.ind = T))
        idx_ppms<-unique(signif_ppms[,2])
        chrqtl<-unique(templateresult[signif_ppms[,1],1])

        if (sum(!(chr %in% chrqtl))>0){
        stop("One or several chromosomes are missing")}
        
        totallength <- getgenomelength(templateresult)
        nchr <- length(unique(templateresult[, 1]))
        cvalues <- circlelocations(totallength + (nchr * spacing))
        drawcirculargenome(templateresult, spacing = spacing, 
            lodmarkers = FALSE)
        ppmaxe<-seq(0,10,by=1)
        drawstraightmetabolome(ppmaxe)



if (missing(chr)){   
colorz <- rainbow(length(chrqtl))

for (y in 1:dim(signif_ppms)[1]){
qtll <- locationtocircle(templateresult, templateresult[signif_ppms[y,1],1], 
                    templateresult[signif_ppms[y,1],2], spacing = spacing)   
points(qtll, col = "red", pch = 19, cex = 0.5)
#text(qtll * 1.1, markers[signif_ppms[y,1]], col = colorz[which(chrqtl==templateresult[signif_ppms[y,1],1])],cex = 0.3,srt=atan(qtll[2]/qtll[1])*(90/pi))
text(qtll * 1.15, rownames(templateresult)[signif_ppms[y,1]], col = colorz[which(chrqtl==templateresult[signif_ppms[y,1],1])],cex = 0.3,srt=atan(qtll[2]/qtll[1])*(90/pi))


traitl<-locationtoaxis(ppmt[signif_ppms[y,2]])
points(traitl, col = colorz[which(chrqtl==templateresult[signif_ppms[y,1],1])], pch = 24, cex = 0.8)

drawspline(traitl, qtll, col = colorz[which(chrqtl==templateresult[signif_ppms[y,1],1])])

#points(qtll * (1 + 0.1 * ((1/length(templateresult)))), col = "red", pch = 19, cex = 1)
text(t(c(traitl[1,1], 0.06 )),paste(round(ppmt[signif_ppms[y,2]],digits=2)),cex=0.4,col="black",srt=90)

}
}else{
signif_ppmsChr<-data.frame()
chr<-sort(chr)
for (c in 1:length(chr)){
signif_ppmsChr<-rbind(signif_ppmsChr,signif_ppms[which(templateresult[signif_ppms[,1],1]==chr[c]),])
}
idx_ppmsChr<-unique(signif_ppmsChr[,2])
colorz <- rainbow(length(chr))

for (y in 1:dim(signif_ppmsChr)[1]){
qtll <- locationtocircle(templateresult, templateresult[signif_ppmsChr[y,1],1], 
                    templateresult[signif_ppmsChr[y,1],2], spacing = spacing)   
points(qtll, col = "red", pch = 19, cex = 0.5)
#text(qtll * 1.1, markers[signif_ppmsChr[y,1]], col = colorz,cex = 0.3,srt=atan(qtll[2]/qtll[1])*(90/pi))
text(qtll * 1.15, rownames(templateresult)[signif_ppmsChr[y,1]], col = colorz[which(chr==templateresult[signif_ppmsChr[y,1],1])],cex = 0.3,srt=atan(qtll[2]/qtll[1])*(90/pi))


traitl<-locationtoaxis(ppmt[signif_ppmsChr[y,2]])
points(traitl, col = colorz[which(chr==templateresult[signif_ppmsChr[y,1],1])], pch = 24, cex = 0.8)

drawspline(traitl, qtll, col = colorz[which(chr==templateresult[signif_ppmsChr[y,1],1])])

#points(qtll * (1 + 0.1 * ((1/length(templateresult)))), col = "red", pch = 19, cex = 1)
text(t(c(traitl[1,1], 0.06 )),paste(round(ppmt[signif_ppmsChr[y,2]],digits=2)),cex=0.4,col="black",srt=90)
}
}

legend("topleft", c("Trait", "mQTL"), col = c("black","black"), pch = c(24, 19), cex = 0.6)

}
