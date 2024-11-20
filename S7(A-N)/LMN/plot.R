load(paste(substr(getwd(),1,stringr::str_length(getwd())-4),"/support/MDM_421.RData",sep=""))
library(ggplot2)
{colorset=c("#7AC39b","#d25b9c","#6bb4d4")
d<-tsne_result
c<-as.character(info_421$group4)
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



pdata<-data.frame(ID=info_421$sample[which(!duplicated(info_421$sample))],
                  Type=info_421$group3[which(!duplicated(info_421$sample))],
                  M1=0,M2=0)
for (i in 1:12)
{
  pdata[i,3]=length(which(info$sample==pdata[i,1]&cl_i$cluster==2))/
    length(which(info$sample==pdata[i,1]))
  pdata[i,4]=length(which(info$sample==pdata[i,1]&cl_i$cluster==1))/
    length(which(info$sample==pdata[i,1]))
}
# pdata=pdata[-c(7,8),]
{adbl<-pdata$M1[which(pdata$Type=="B_C")]
  adcd<-pdata$M1[which(pdata$Type=="B_B")]
  wtcd<-pdata$M1[which(pdata$Type=="B_A")]
  
  # t3<-t.test(adbl,wtcd)[["p.value"]]
  # t2<-t.test(adbl,adcd)[["p.value"]]
  # t1<-t.test(adcd,wtcd)[["p.value"]]
  
  t3<-wilcox.test(adbl,wtcd)[["p.value"]]
  t2<-wilcox.test(adbl,adcd)[["p.value"]]
  t1<-wilcox.test(adcd,wtcd)[["p.value"]]
  
  
  picdata<-data.frame(
    Group=factor(c(
      rep("WT_CD",length(wtcd)),
      rep("AD_CD",length(adcd)),rep("AD_BL",length(adbl))
    ),levels = c("WT_CD","AD_CD","AD_BL")),
    mdata=c(wtcd,adcd,adbl))
  color=c(rep("#4d869e",length(wtcd)),
          rep("#cc82a5",length(adcd)),rep("#CE1256",length(adbl)))
  P <- ggplot(picdata,aes(x=Group,y=mdata))+ 
    stat_boxplot(geom = "errorbar",width=0.15,color=c("#4d869e","#cc82a5","#CE1256"))+ #由于自带的箱形图没有胡须末端没有短横线，使用误差条的方式补上
    geom_boxplot(size=0.7,fill="white",color=c("#4d869e","#cc82a5","#CE1256"))+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
    geom_jitter(width =0.1,shape = 21,size=2,stroke=1,color=color)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
    theme_bw()+ #背景变为白色
    theme(legend.position="none", #不需要图例
          panel.grid.major = element_blank(), #不显示网格线
          panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.02,
                                                   max(unlist(picdata$mdata))*1.14)+
    ylab("")+xlab("") #设置x轴和y轴的标题
  if (!is.nan(t1))
  {
    P=P+annotate("segment", x = 0.9, xend = 2.1, y = max(unlist(picdata$mdata))*1.01,
                 yend = max(unlist(picdata$mdata))*1.01,size = 1)
    if (t1<0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.02, label="***",size=8)
    if (t1<0.01&t1>=0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.02, label="**",size=8)
    if (t1<=0.05&t1>=0.01)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.02, label="*",size=8)
    if (t1>0.05)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.04, label="ns",size=4)
  }
  if (!is.nan(t2))
  {
    P=P+annotate("segment", x = 1.9, xend = 3.1, y = max(unlist(picdata$mdata))*1.04,
                 yend = max(unlist(picdata$mdata))*1.04,size = 1)
    if (t2<0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.05, label="***",size=8)
    if (t2<0.01&t2>=0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.05, label="**",size=8)
    if (t2<=0.05&t2>=0.01)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.05, label="*",size=8)
    if (t2>0.05)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.07, label="ns",size=4)
  }
  if (!is.nan(t3))
  {
    P=P+annotate("segment", x = 1, xend = 3, y = max(unlist(picdata$mdata))*1.11,
                 yend = max(unlist(picdata$mdata))*1.11,size = 1)
    if (t3<0.001)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.12, label="***",size=8)
    if (t3<0.01&t3>=0.001)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.12, label="**",size=8)
    if (t3<=0.05&t3>=0.01)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.12, label="*",size=8)
    if (t3>0.05)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.14, label="ns",size=4)
  }
  ggsave(paste(getwd(),"/M1_BOXPLOT.pdf",sep=""),
         width = 3.5,height = 3.5,units = "in")
}
{adbl<-pdata$M2[which(pdata$Type=="B_C")]
  adcd<-pdata$M2[which(pdata$Type=="B_B")]
  wtcd<-pdata$M2[which(pdata$Type=="B_A")]
  
  # t3<-t.test(adbl,wtcd)[["p.value"]]
  # t2<-t.test(adbl,adcd)[["p.value"]]
  # t1<-t.test(adcd,wtcd)[["p.value"]]
  
  t3<-wilcox.test(adbl,wtcd)[["p.value"]]
  t2<-wilcox.test(adbl,adcd)[["p.value"]]
  t1<-wilcox.test(adcd,wtcd)[["p.value"]]
  
  
  picdata<-data.frame(
    Group=factor(c(
      rep("WT_CD",length(wtcd)),
      rep("AD_CD",length(adcd)),rep("AD_BL",length(adbl))
    ),levels = c("WT_CD","AD_CD","AD_BL")),
    mdata=c(wtcd,adcd,adbl))
  color=c(rep("#4d869e",length(wtcd)),
          rep("#cc82a5",length(adcd)),rep("#CE1256",length(adbl)))
  P <- ggplot(picdata,aes(x=Group,y=mdata))+ 
    stat_boxplot(geom = "errorbar",width=0.15,color=c("#4d869e","#cc82a5","#CE1256"))+ #由于自带的箱形图没有胡须末端没有短横线，使用误差条的方式补上
    geom_boxplot(size=0.7,fill="white",color=c("#4d869e","#cc82a5","#CE1256"))+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
    geom_jitter(width =0.1,shape = 21,size=2,stroke=1,color=color)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
    theme_bw()+ #背景变为白色
    theme(legend.position="none", #不需要图例
          panel.grid.major = element_blank(), #不显示网格线
          panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.02,
                                                   max(unlist(picdata$mdata))*1.14)+
    ylab("")+xlab("") #设置x轴和y轴的标题
  if (!is.nan(t1))
  {
    P=P+annotate("segment", x = 0.9, xend = 2.1, y = max(unlist(picdata$mdata))*1.01,
                 yend = max(unlist(picdata$mdata))*1.01,size = 1)
    if (t1<0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.02, label="***",size=8)
    if (t1<0.01&t1>=0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.02, label="**",size=8)
    if (t1<=0.05&t1>=0.01)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.02, label="*",size=8)
    if (t1>0.05)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.04, label="ns",size=4)
  }
  if (!is.nan(t2))
  {
    P=P+annotate("segment", x = 1.9, xend = 3.1, y = max(unlist(picdata$mdata))*1.04,
                 yend = max(unlist(picdata$mdata))*1.04,size = 1)
    if (t2<0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.05, label="***",size=8)
    if (t2<0.01&t2>=0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.05, label="**",size=8)
    if (t2<=0.05&t2>=0.01)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.05, label="*",size=8)
    if (t2>0.05)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.07, label="ns",size=4)
  }
  if (!is.nan(t3))
  {
    P=P+annotate("segment", x = 1, xend = 3, y = max(unlist(picdata$mdata))*1.11,
                 yend = max(unlist(picdata$mdata))*1.11,size = 1)
    if (t3<0.001)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.12, label="***",size=8)
    if (t3<0.01&t3>=0.001)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.12, label="**",size=8)
    if (t3<=0.05&t3>=0.01)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.12, label="*",size=8)
    if (t3>0.05)
      P=P+annotate("text", x = 2, y = max(unlist(picdata$mdata))*1.14, label="ns",size=4)
  }
  ggsave(paste(getwd(),"/M2_BOXPLOT.pdf",sep=""),
         width = 3.5,height = 3.5,units = "in")
}

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