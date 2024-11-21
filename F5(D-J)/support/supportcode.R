# This is support code to generate MDM_5.RData, MDM_421.RData and rmk_umap.RData.

#load serum original data
load("C:/Users/li08/Desktop/barley/cytof/serum/data_serum.RData")

library(umap)
nao_umap<- umap(data_serum,preserve.seed = TRUE, method = c("naive"))
library(ggplot2)
centers=20
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
cl_i <- kmeans(data_serum, centers = centers, nstart = 3,algorithm="Lloyd")

colorset=c(
  "#80f1f3","#67001f","#fccde5","#bc80ed","#1c808d",
  "#0571b0","#3c0030","#303c00","#00671f","#bf2000",
  "#ff1f99","#f15128","#1ddfc7","#1eaafa","#1b8072",
  "#ffff99","#b15928","#8dd3c7","#bebada","#fb8072",
  "#80f1f3","#67001f","#fccde5","#bc80ed","#1c808d",
  "#0571b0","#3c0030","#303c00","#00671f","#bf2000",
  "#ff1f99","#f15128","#1ddfc7","#1eaafa","#1b8072",
  "#1011f3","#17332f","#1ccde5","#fc80ed","#fb1012"
)
# tsne_result$umap1[which(tsne_result$umap1 < -20)]=-20
# tsne_result$umap2[which(tsne_result$umap2 >20)]=20
p1<-ggplot(tsne_result[,],aes(umap1,umap2,color=as.character(cl_i$cluster))) +
  geom_point() +scale_color_manual(values = colorset[1:centers])
#ggsave(paste("C:/Users/li08/Desktop/barley/cytof/serum/",centers,"-umap.png",sep=""),
#       p1,units="in",width = 10,height = 10,dpi=300)

pdata<-data_serum[1,]
for (i in 1:centers)
{
  pdata<-rbind(pdata,apply(data_serum[which(cl_i$cluster==i),],2,median))
  rownames(pdata)[i+1]=paste("Cluster_",i,sep="")
}
pdata=pdata[-1,]
library(pheatmap)
# pheatmap(pdata[,],scale = "row",show_colnames = T,show_rownames = T,
#          color = colorRampPalette(c("#055070", "white","#ae1266"))(50),
#          annotation_names_row = TRUE,cluster_cols = F,cluster_rows = T,
#          filename = paste("C:/Users/li08/Desktop/barley/cytof/serum/",centers,"-umap_heatmap.png",sep=""),
#          width=12,height = 0.45*centers)
pdata<-data.frame(A=rep(0,centers),B=rep(0,centers),C=rep(0,centers))
for (i in 1:centers)
{
  pdata$A[i]=length(which(info_serum$group3=="B_A"&cl_i$cluster==i))
  pdata$B[i]=length(which(info_serum$group3=="B_B"&cl_i$cluster==i))
  pdata$C[i]=length(which(info_serum$group3=="B_C"&cl_i$cluster==i))
}
#write.csv(pdata,paste("C:/Users/li08/Desktop/barley/cytof/serum/",centers,"—umap.csv",sep=""))
rm(p1,pdata,tsne_result,colorset,i)

info_serum$group4[which(cl_i$cluster==1)]="B cells"
info_serum$group4[which(cl_i$cluster==2)]="B cells"
info_serum$group4[which(cl_i$cluster==3)]="CD8+T cells"
info_serum$group4[which(cl_i$cluster==4)]="NK Cells"
info_serum$group4[which(cl_i$cluster==5)]="DCs"
info_serum$group4[which(cl_i$cluster==6)]="Neutrophil"
info_serum$group4[which(cl_i$cluster==7)]="CD4+T cells"
info_serum$group4[which(cl_i$cluster==8)]="DCs"
info_serum$group4[which(cl_i$cluster==9)]="MDM"
info_serum$group4[which(cl_i$cluster==10)]="B cells"
info_serum$group4[which(cl_i$cluster==11)]="B cells"
info_serum$group4[which(cl_i$cluster==12)]="B cells"
info_serum$group4[which(cl_i$cluster==13)]="B cells"
info_serum$group4[which(cl_i$cluster==14)]="B cells"
info_serum$group4[which(cl_i$cluster==15)]="CD8+T cells"
info_serum$group4[which(cl_i$cluster==16)]="CD4+T cells"
info_serum$group4[which(cl_i$cluster==17)]="CD8+T cells"
info_serum$group4[which(cl_i$cluster==18)]="Monocytes"
info_serum$group4[which(cl_i$cluster==19)]="B cells"
info_serum$group4[which(cl_i$cluster==20)]="Neutrophil"
colorset=c(
  "#ff1f99","#f15128","#1b8072",
  "#1011f3","#17332f","#1ccde5","#67001f","#fb1012"
)
library(ggplot2)
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
p1<-ggplot(tsne_result[,],aes(umap1,umap2,color=info_serum$group4)) +
  geom_point(size=0.1) +scale_color_manual(values = colorset[1:8])
# ggsave(paste("C:/Users/li08/Desktop/barley/cytof/serum/rmk-umap.png",sep=""),
#        p1,units="in",width = 10,height = 10,dpi=300)

save.image("C:/Users/li08/Desktop/barley/cytof/serum/rmk_umap.RData")






rm(cl_i,nao_umap)
id="MDM"
library(umap)
library(ggplot2)
for (idi in 1:length(id))
{
  data=data_serum[which(info_serum$group4==id[idi]),]
  info=info_serum[which(info_serum$group4==id[idi]),]
  nao_umap<- umap(data[,c(33,4,30,11,15,22,20)],preserve.seed = TRUE, method = c("naive"))
  # data=data[which(nao_umap$layout[,1]<20&nao_umap$layout[,2]>-5),]
  # info=info[which(nao_umap$layout[,1]<20&nao_umap$layout[,2]>-5),]
  # nao_umap$layout=nao_umap$layout[which(nao_umap$layout[,1]<20&nao_umap$layout[,2]>-5),]
  centers=5
  {
    tsne_result = data.frame(nao_umap$layout)
    colnames(tsne_result) = c("umap1","umap2")
    cl_i <- kmeans(data[,c(33,4,30,11,15,22,20)], centers = centers, nstart = 3,algorithm="Lloyd")
    colorset=c(
      "#ff1f99","#f15128","#1ddfc7","#1eaafa","#1b8072",
      "#1011f3","#17332f","#1ccde5","#fc80ed","#fb1012"
    )
    p1<-ggplot(tsne_result[,],aes(umap1,umap2,color=as.character(cl_i$cluster))) +
      geom_point() +scale_color_manual(values = colorset[1:centers])
    # ggsave(paste("C:/Users/li08/Desktop/barley/Fcytof/re_umap/",id[idi],"_",centers,"_umap.png",sep=""),
    #        p1,units="in",width = 10,height = 10,dpi=300)
    
    pdata<-data[1,]
    for (i in 1:centers)
    {
      pdata<-rbind(pdata,apply(data[which(cl_i$cluster==i),],2,median))
      rownames(pdata)[i+1]=paste("Cluster_",i,sep="")
    }
    pdata=pdata[-1,]
    library(pheatmap)
    # pheatmap(pdata[,],scale = "row",show_colnames = T,show_rownames = T,
    #          color = colorRampPalette(c("#055070", "white","#ae1266"))(50),
    #          annotation_names_row = TRUE,cluster_cols = F,cluster_rows = T,
    #          filename = paste("C:/Users/li08/Desktop/barley/Fcytof/re_umap/",id[idi],"_",centers,"-f_umap_heatmap.png",sep=""),
    #          width=12,height = 2+centers*0.5)
    pdata<-data.frame(A=rep(0,centers),B=rep(0,centers),C=rep(0,centers))
    for (i in 1:centers)
    {
      pdata$A[i]=length(which(info$group3=="B_A"&cl_i$cluster==i))
      pdata$B[i]=length(which(info$group3=="B_B"&cl_i$cluster==i))
      pdata$C[i]=length(which(info$group3=="B_C"&cl_i$cluster==i))
    }
    # write.csv(pdata,paste("C:/Users/li08/Desktop/barley/Fcytof/re_umap/",id[idi],"_",centers,"—f_umap.csv",sep=""))
    save.image("C:/Users/li08/Desktop/barley/cytof/serum/MDM_5.RData")
  }
}







library(umap)
library(ggplot2)
colorset=c(
  "#f16158","#7AC39b",
  "#e2a067","#7d5094","#d25b9c","#6bb4d4","#f066d9","#5b6bce"
)
info$group4=cl_i$cluster
data_421=data[which(info$group4==2|info$group4==1|info$group4==4),c(33,30,15)]
info_421=info[which(info$group4==2|info$group4==1|info$group4==4),]
info_421$group4[which(info_421$group4==2)]="M1"
info_421$group4[which(info_421$group4==4)]="M0"
info_421$group4[which(info_421$group4==1)]="M2"
nao_umap<- umap(data_421,preserve.seed = TRUE, method = c("naive"))
del_id=which(info_421$group4=="M0"&nao_umap$layout[,2]>-5)
info_421=info_421[-del_id,]
data_421=data_421[-del_id,]
nao_umap<- umap(data_421,preserve.seed = TRUE, method = c("naive"))
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
save.image("C:/Users/li08/Desktop/barley/cytof/serum/MDM_421.RData")