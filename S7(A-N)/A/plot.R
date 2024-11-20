load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/rmk_umap.RData",sep=""))
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
ggsave(paste(getwd(),"/umap.png",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)

color=info_serum$group4[which(!duplicated(info_serum$group4))]
p1<-ggplot(tsne_result[which(!duplicated(info_serum$group4)),],aes(umap1,umap2,color=color)) +
  geom_point(size=2) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/label_umap.pdf",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)



tsne_result = as.data.frame(nao_umap$layout)[which(info_serum$group3=="B_A"),]
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.2, geom = "polygon",
                  colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()
ggsave(paste(getwd(),"/WTCD.pdf",sep=""),p1,units="in",width = 4,height = 3)

tsne_result = as.data.frame(nao_umap$layout)[which(info_serum$group3=="B_B"),]
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.2, geom = "polygon",
                  colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()
ggsave(paste(getwd(),"/ADCD.pdf",sep=""),p1,units="in",width = 4,height = 3)

tsne_result = as.data.frame(nao_umap$layout)[which(info_serum$group3=="B_C"),]
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.2, geom = "polygon",
                  colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()
ggsave(paste(getwd(),"/ADBL.pdf",sep=""),p1,units="in",width = 4,height = 3)
