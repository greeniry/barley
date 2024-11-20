load(paste(substr(getwd(),1,stringr::str_length(getwd())-3),"/support/rmk_umap.RData",sep=""))
data=data_serum
info=info_serum
rm(nao_umap,cl_i,data_serum,info_serum)
library(ggplot2)
id=unique(info$group4)
pdata<-data[1,]
for (i in 1:length(id))
{
  pdata<-rbind(pdata,apply(data[which(info$group4==id[i]),],2,median))
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
         filename = paste(getwd(),"heatmap.pdf",sep=""),
         width=9,height = 2.5)
