load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/d_rmk_del_jiaozhi.RData",sep=""))
library(ggplot2)
tsne_result = as.data.frame(nao_umap$layout)[1:66459,]
colnames(tsne_result) = c("umap1","umap2")
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.1, geom = "polygon",
                  colour = "ivory", alpha = 1, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#7Ac89b",high="#e2a057")+theme_bw()+xlim(-5,5)+ylim(-5.5,7)
ggsave(paste(getwd(),"/DENSITY_HOM.pdf",sep=""),p1,units="in",width = 4,height = 3)

tsne_result = as.data.frame(nao_umap$layout)[66460:358996,]
colnames(tsne_result) = c("umap1","umap2")
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.1, geom = "polygon",
                  colour = "ivory", alpha = 1, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()+xlim(-5,5)+ylim(-5.5,7)
ggsave(paste(getwd(),"/DENSITY_ACT.pdf.pdf",sep=""),p1,units="in",width = 4,height = 3)

