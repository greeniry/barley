library(ggplot2)
load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/del_new_rmk.RData",sep=""))
id=unique(new_info_nao$group3)
pdata<-new_data_nao[1,]
for (i in 1:length(id))
{
  pdata<-rbind(pdata,apply(new_data_nao[which(new_info_nao$group3==id[i]),],2,median))
  rownames(pdata)[i+1]=id[i]
}
pdata=pdata[-1,]
pdata=scale(pdata)
library(pheatmap)
rm=pdata[,is.na(pdata[1,])]
pdata=pdata[,!is.na(pdata[1,])]
pdata=t(scale(t(pdata)))
pdata=cbind(pdata,rm)
pdata=pdata[,order(colnames(pdata))]
pheatmap(pdata,scale = "row",show_colnames = T,show_rownames = T,
         color = colorRampPalette(c("#055070", "white","#ae1266"))(100),
         annotation_names_row = TRUE,cluster_cols = F,cluster_rows = T,
         filename = paste(getwd(),"/heatmap.pdf",sep=""),
         width=9,height = 2.5)
