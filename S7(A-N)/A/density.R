load("C:/Users/li08/Desktop/barley/Fcytof/serum/rmk_umap.RData")
library(ggplot2)
tsne_result = as.data.frame(nao_umap$layout)[which(info_serum$group3=="B_A"),]
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.2, geom = "polygon",
                  colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()
ggsave("C:/Users/li08/Desktop/barley/Fcytof/serum/mk_WTCD.pdf",p1,units="in",width = 4,height = 3)

tsne_result = as.data.frame(nao_umap$layout)[which(info_serum$group3=="B_B"),]
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.2, geom = "polygon",
                  colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()
ggsave("C:/Users/li08/Desktop/barley/Fcytof/serum/mk_ADCD.pdf",p1,units="in",width = 4,height = 3)

tsne_result = as.data.frame(nao_umap$layout)[which(info_serum$group3=="B_C"),]
colnames(tsne_result) = c("umap1","umap2")
tsne_result=-tsne_result
p1<-ggplot(tsne_result) +
  stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
                  linewidth = 0.2, geom = "polygon",
                  colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
  scale_fill_gradient(low="#6bb4d4",high="#7d5094")+theme_bw()
ggsave("C:/Users/li08/Desktop/barley/Fcytof/serum/mk_ADBL.pdf",p1,units="in",width = 4,height = 3)








# library(umap)
# load("C:/Users/li08/Desktop/barley/cytof/d_f_umap_naive.RData")
# library(ggplot2)
# tsne_result = as.data.frame(nao_umap$layout)[which(f_info_nao$group4=="T_A"),]
# colnames(tsne_result) = c("umap1","umap2")
# 
# p1<-ggplot(tsne_result) +
#   stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
#                   linewidth = 0.2, geom = "polygon",
#                   colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
#   scale_fill_gradient(low="#055070",high="#ae1266")+theme_bw()
# ggsave("C:/Users/li08/Desktop/barley/Fcytof/C/WTCD.pdf",p1,units="in",width = 4,height = 3)
# 
# tsne_result = as.data.frame(nao_umap$layout)[which(f_info_nao$group4=="T_B"),]
# colnames(tsne_result) = c("umap1","umap2")
# 
# p1<-ggplot(tsne_result) +
#   stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
#                   linewidth = 0.2, geom = "polygon",
#                   colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
#   scale_fill_gradient(low="#055070",high="#ae1266")+theme_bw()
# ggsave("C:/Users/li08/Desktop/barley/Fcytof/C/ADCD.pdf",p1,units="in",width = 4,height = 3)
# 
# tsne_result = as.data.frame(nao_umap$layout)[which(f_info_nao$group4=="T_C"),]
# colnames(tsne_result) = c("umap1","umap2")
# 
# p1<-ggplot(tsne_result) +
#   stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
#                   linewidth = 0.2, geom = "polygon",
#                   colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
#   scale_fill_gradient(low="#055070",high="#ae1266")+theme_bw()
# ggsave("C:/Users/li08/Desktop/barley/Fcytof/C/ADBL.pdf",p1,units="in",width = 4,height = 3)




# load("C:/Users/li08/Desktop/barley/Fcytof/C/d_new_mianyi.RData")
# library(ggplot2)
# tsne_result = as.data.frame(nao_umap$layout)[which(new_info_nao$group4=="T_A"),]
# colnames(tsne_result) = c("umap1","umap2")
# tsne_result=-tsne_result
# p1<-ggplot(tsne_result) +
#   stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
#                   linewidth = 0.2, geom = "polygon",
#                   colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
#   scale_fill_gradient(low="#055070",high="#ae1266")+theme_bw()
# ggsave("C:/Users/li08/Desktop/barley/Fcytof/C/new_WTCD.pdf",p1,units="in",width = 4,height = 3)
# 
# tsne_result = as.data.frame(nao_umap$layout)[which(new_info_nao$group4=="T_B"),]
# colnames(tsne_result) = c("umap1","umap2")
# tsne_result=-tsne_result
# p1<-ggplot(tsne_result) +
#   stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
#                   linewidth = 0.2, geom = "polygon",
#                   colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
#   scale_fill_gradient(low="#055070",high="#ae1266")+theme_bw()
# ggsave("C:/Users/li08/Desktop/barley/Fcytof/C/new_ADCD.pdf",p1,units="in",width = 4,height = 3)
# 
# tsne_result = as.data.frame(nao_umap$layout)[which(new_info_nao$group4=="T_C"),]
# colnames(tsne_result) = c("umap1","umap2")
# tsne_result=-tsne_result
# p1<-ggplot(tsne_result) +
#   stat_density_2d(aes(x = umap1, y =umap2, fill = after_stat(nlevel)), 
#                   linewidth = 0.2, geom = "polygon",
#                   colour = "ivory", alpha = 0.8, n = 150, h = c(1.2, 1.2))+
#   scale_fill_gradient(low="#055070",high="#ae1266")+theme_bw()
# ggsave("C:/Users/li08/Desktop/barley/Fcytof/C/new_ADBL.pdf",p1,units="in",width = 4,height = 3)