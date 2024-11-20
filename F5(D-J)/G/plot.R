load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/MDM_421.RData",sep=""))
library(ggplot2)
{colorset=c("#7AC39b","#d25b9c","#6bb4d4")
d<-tsne_result[-which(info_421$sample=="BM0010"|info_421$sample=="BM0008"),]
c<-as.character(info_421$group4)[-which(info_421$sample=="BM0010"|info_421$sample=="BM0008")]
p1<-ggplot(d,aes(umap1,umap2,color=c)) +theme_classic()+
  geom_point(size=0.2) +scale_color_manual(values = colorset)
ggsave(paste(getwd(),"/",421,"_umap.png",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)
d<-tsne_result[which(!duplicated(info_421$group4)),]
c<-as.character(info_421$group4)[which(!duplicated(info_421$group4))]
p1<-ggplot(d,aes(umap1,umap2,color=c)) +theme_classic()+geom_point(size=2) +scale_color_manual(values = colorset)
ggsave(paste(getwd(),"/",421,"label","_umap.pdf",sep=""),
       p1,units="in",width = 5,height = 3,dpi=300)
}

info_421=info_421[-which(info_421$sample=="BM0010"|info_421$sample=="BM0008"),]
colorset=c("#6bb4d4","#d25b9c","#7AC39b")
pdata=data.frame(type=factor(rep(c("WT CD","AD CD","AD BL"),3),levels = c("WT CD","AD CD","AD BL")),
                 cell=factor(rep(c("M2","M1","M0"),each=3),levels = c("M2","M1","M0")),
                 counts=c(
                   length(which(info_421$group3=="B_A"&info_421$group4=="M2"))/length(which(info_421$group3=="B_A"))*100,
                   length(which(info_421$group3=="B_B"&info_421$group4=="M2"))/length(which(info_421$group3=="B_B"))*100,
                   length(which(info_421$group3=="B_C"&info_421$group4=="M2"))/length(which(info_421$group3=="B_C"))*100,
                   length(which(info_421$group3=="B_A"&info_421$group4=="M1"))/length(which(info_421$group3=="B_A"))*100,
                   length(which(info_421$group3=="B_B"&info_421$group4=="M1"))/length(which(info_421$group3=="B_B"))*100,
                   length(which(info_421$group3=="B_C"&info_421$group4=="M1"))/length(which(info_421$group3=="B_C"))*100,
                   length(which(info_421$group3=="B_A"&info_421$group4=="M0"))/length(which(info_421$group3=="B_A"))*100,
                   length(which(info_421$group3=="B_B"&info_421$group4=="M0"))/length(which(info_421$group3=="B_B"))*100,
                   length(which(info_421$group3=="B_C"&info_421$group4=="M0"))/length(which(info_421$group3=="B_C"))*100
                 ))
ggplot(pdata, aes(x=type,y=counts,fill=cell)) +
  geom_col(position = 'fill')+theme_classic()+
  scale_fill_manual(values=colorset)