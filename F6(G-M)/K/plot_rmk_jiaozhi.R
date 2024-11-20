load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/d_rmk_del_jiaozhi.RData",sep=""))
library(ggplot2)
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
tsne_result<-cbind(tsne_result,type=c(rep("Homeostatic",66459),rep("Activated",292537)))
t<-intersect(DAM_id,activated_id)
id=c(0)
for (i in 1:length(t))
{
  id=c(id,which(activated_id==t[i]))
}
id=id[-1]
tsne_result$type[id+66459]="DAM"
colorset=c("#dddddd","#ae1266")
p1<-ggplot(tsne_result[358568:66460,],aes(umap1,umap2,color=type)) +
  geom_point(size=0.1) +scale_color_manual(values = colorset)+theme_classic()
ggsave(paste(getwd(),"/jiaozhi.png",sep=""),
       p1,units="in",width = 5,height = 4,dpi=300)


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
  "#dddddd","#dddddd",
  "#ae1266","#ae1266","#dddddd","#dddddd","#dddddd","#dddddd"
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

