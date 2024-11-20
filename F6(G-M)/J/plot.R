load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/ACT_10_umap.RData",sep=""))
rm(jiaozhi_data_nao,jiaozhi_info_nao,nao_umap)
info$group3[which(cl_i$cluster==1)]="MHCII"
info$group3[which(cl_i$cluster==2)]="MHCII"
info$group3[which(cl_i$cluster==3)]="Cycling"
info$group3[which(cl_i$cluster==4)]="DAM2"
info$group3[which(cl_i$cluster==5)]="Cycling"
info$group3[which(cl_i$cluster==6)]="TM"
info$group3[which(cl_i$cluster==7)]="DAM1"
info$group3[which(cl_i$cluster==8)]="Ly_6C"
info$group3[which(cl_i$cluster==9)]="DAM2"
info$group3[which(cl_i$cluster==10)]="CD95+"
library(ggplot2)

colorset=c(
  "#f16158","#7AC39b",
  "#f066d9","#7d5094","#e2a067","#6bb4d4","#5b6bce","#d25b9c"
)
shunxu<-c(which(info$group3=="CD95+"),which(info$group3=="Cycling"),which(info$group3=="Ly_6C"),
          which(info$group3=="MHCII"),which(info$group3=="TM"),which(info$group3=="DAM1"),
          which(info$group3=="DAM2"))
p1<-ggplot(tsne_result[shunxu,],aes(umap1,umap2,color=info$group3[shunxu])) +
  geom_point(size=0.2) +scale_color_manual(values = colorset[1:10])+theme_classic()
ggsave(paste(getwd(),"/act_umap.png",sep=""),
       p1,units="in",width = 6,height = 6,dpi=300)
p1<-ggplot(tsne_result[which(!duplicated(info$group3)),],
           aes(umap1,umap2,color=info$group3[which(!duplicated(info$group3))])) +
  geom_point(size=2) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/label_umap.png",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)


