load(paste(substr(getwd(),1,stringr::str_length(getwd())-3),"/support/rmk_umap.RData",sep=""))
library(ggplot2)
tsne_result = as.data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
colorset=c(
  "#6bb4d4","#f16158","#7AC39b",
  "#e2a067","#7d5094","#d25b9c","#f066d9","#5b6bce"
)
p1<-ggplot(tsne_result,aes(umap1,umap2,color=info_serum$group4)) +
  geom_point(size=0.1) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"umap.pdf",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)

color=info_serum$group4[which(!duplicated(info_serum$group4))]
p1<-ggplot(tsne_result[which(!duplicated(info_serum$group4)),],aes(umap1,umap2,color=color)) +
  geom_point(size=2) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"label_umap.pdf",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)

