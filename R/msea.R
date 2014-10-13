msea <-
function(total,sample,description,method){

total=unique(total[,c(1,2)])
names(total)=c("id","class")
names(description)=c("class","description")

#prepare total class summary
total=unique(total)
class=total$class
class_summary=as.data.frame.table(sort(table(class)))
names(class_summary)=c("class","n")

total_id_number=length(unique(total$id))

#prepare sample class summary
sample_total=total[which(total$id %in% sample),]
sample_class=sample_total$class
sample_class_summary=as.data.frame.table(sort(table(sample_class)))
names(sample_class_summary)=c("class","n")

sample_id_number=length(unique(sample_total$id))

#prepare contingency table

contingency=merge(class_summary,sample_class_summary,by.x="class",by.y="class")
names(contingency)=c("class","total","sample")

contingency_p_value=unlist(lapply(1:length(contingency$class),function(x){fisher.test(matrix(c(contingency$total[x],total_id_number-contingency$total[x],contingency$sample[x],sample_id_number-contingency$sample[x]),nrow=2),alternative="less")$p.value}))
contingency_p_value_adj=p.adjust(contingency_p_value,method=method)

odds=(contingency$sample/sample_id_number)/(contingency$total/total_id_number)

result=cbind(as.data.frame(contingency),odds,contingency_p_value,contingency_p_value_adj)
names(result)=c("class","total","sample","odds","pval","pval_adjust_BH")

result=merge(result,description,by.x="class",by.y="class")

result
}
