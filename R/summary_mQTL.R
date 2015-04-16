summary_mQTL <-
function(results,redfile,Th=5){
# Function to summarize the results of a all the runs and their differences
# Input:  2D results of LOD scores
# Output: Summaries
 
    print(paste("Summarize single runs",gsub(".ppm", "",redfile)))
    summa<-summarize(results,redfile,Th)
    print(summa)
    write.table(summa, file=paste("signif",gsub(".ppm", "","dat",redfile),sep="."),quote=FALSE,sep="\t")
#  print("Prepare the pairs")

}
