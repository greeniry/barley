load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/MDM_5.RData",sep=""))
library(ggplot2)
colorset=c(
  "#f16158","#7AC39b",
  "#e2a067","#7d5094","#d25b9c","#6bb4d4","#f066d9","#5b6bce"
)
info$group4=cl_i$cluster
data=data[-which(info$sample=="BM0010"|info$sample=="BM0008"),]
info=info[-which(info$sample=="BM0010"|info$sample=="BM0008"),]
data_21=data[which(info$group4==2|info$group4==1),c(33,30,15)]
info_21=info[which(info$group4==2|info$group4==1),]
info_21$group4[which(info_21$group4==2)]="M1"
info_21$group4[which(info_21$group4==1)]="M2"
nao_umap<- umap(data_21,preserve.seed = TRUE, method = c("naive"))
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
colorset=c("#d25b9c","#6bb4d4")
d<-tsne_result
c<-as.character(info_21$group4)
p1<-ggplot() +
  geom_point(data=d,size=0.7,aes(x=umap2,y=umap1,color=c)) +
  scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/",21,"all","_umap.png",sep=""),
       p1,units="in",width = 4,height = 3,dpi=300)
# p1<-ggplot(d,aes(umap2,umap1,color=c)) +
#   geom_point(size=0.7) +scale_color_manual(values = colorset)+theme_classic()
# ggsave(paste("C:/Users/li08/Desktop/barley/Fcytof/巨噬细胞/",21,"all","_umap.png",sep=""),
#        p1,units="in",width = 4,height = 3,dpi=300)
d2<-tsne_result[which(info_21$group3=="B_A"),]
c2<-as.character(info_21$group4)[which(info_21$group3=="B_A")]
p1<-ggplot() +geom_point(data=d,size=0.7,aes(x=umap2,y=umap1),color="#dddddd") +
  geom_point(data=d2,size=0.7,aes(x=umap2,y=umap1,color=c2)) +
  scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/",21,"WTCD","_umap.png",sep=""),
       p1,units="in",width = 4,height = 3,dpi=300)
d2<-tsne_result[which(info_21$group3=="B_B"),]
c2<-as.character(info_21$group4)[which(info_21$group3=="B_B")]
p1<-ggplot() +geom_point(data=d,size=0.7,aes(x=umap2,y=umap1),color="#dddddd") +
  geom_point(data=d2,size=0.7,aes(x=umap2,y=umap1,color=c2)) +
  scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/",21,"ADCD","_umap.png",sep=""),
       p1,units="in",width = 4,height = 3,dpi=300)
d2<-tsne_result[which(info_21$group3=="B_C"),]
c2<-as.character(info_21$group4)[which(info_21$group3=="B_C")]
p1<-ggplot() +geom_point(data=d,size=0.7,aes(x=umap2,y=umap1),color="#dddddd") +
  geom_point(data=d2,size=0.7,aes(x=umap2,y=umap1,color=c2)) +
  scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/",21,"ADBL","_umap.png",sep=""),
       p1,units="in",width = 4,height = 3,dpi=300)