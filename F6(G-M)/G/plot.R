load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/del_new_rmk.RData",sep=""))
library(ggplot2)
tsne_result = as.data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result

colorset=c(
  "#f16158","#7AC39b",
  "#e2a067","#7d5094","#d25b9c","#6bb4d4","#f066d9","#5b6bce"
)
p1<-ggplot(tsne_result,aes(umap1,umap2,color=new_info_nao$group3)) +
  geom_point(size=0.02) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/umap.png",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)
p1<-ggplot(tsne_result[which(!duplicated(new_info_nao$group3)),],
           aes(umap1,umap2,color=new_info_nao$group3[which(!duplicated(new_info_nao$group3))])) +
  geom_point(size=2) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/label_umap.png",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)
