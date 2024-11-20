load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/rmk_feijiaozhi.RData",sep=""))

library(ggplot2)
tsne_result = as.data.frame(feijiaozhi_nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
# f_info_nao$group3="undefined"
# rownames(f_info_nao)=f_info_nao$X
# f_info_nao[as.character(rmk_feijiaozhi_info$X),3]=rmk_feijiaozhi_info$group3
# f_info_nao[as.character(jiaozhi_info_nao$X),3]="Microglia"
colorset=c(
  "#f16158","#7AC39b",
  "#e2a067","#7d5094","#d25b9c","#f066d9","#5b6bce"
)
p1<-ggplot(tsne_result,aes(umap1,umap2,color=feijiaozhi_info_nao$group3)) +
  geom_point(size=0.1) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/umap.png",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)
