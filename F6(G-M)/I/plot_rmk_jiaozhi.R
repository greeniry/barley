load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/d_rmk_del_jiaozhi.RData",sep=""))
library(ggplot2)
pdata<-data.frame(ID=unique(jiaozhi_info_nao$sample),
                  Type=c("ADBL","ADBL","WTCD","ADBL","ADCD","WTCD","ADCD","ADCD","ADBL","WTCD"),
                  activated=0,Homeostatic=0)
for (i in 1:10)
{
  pdata[i,3]=length(intersect(which(jiaozhi_info_nao$sample==pdata[i,1]),activated_id))/
    length(which(jiaozhi_info_nao$sample==pdata[i,1]))
  pdata[i,4]=length(intersect(which(jiaozhi_info_nao$sample==pdata[i,1]),Homeostatic_id))/
    length(which(jiaozhi_info_nao$sample==pdata[i,1]))
}
{adbl<-pdata$activated[which(pdata$Type=="ADBL")]
  adcd<-pdata$activated[which(pdata$Type=="ADCD")]
  wtcd<-pdata$activated[which(pdata$Type=="WTCD")]
  print(mean(adcd))
  print(sd(adcd))
  print(mean(adbl))
  print(sd(adbl))
  t3<-wilcox.test(adbl,wtcd,correct = F,exact = F)[["p.value"]]
  t2<-wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
  t1<-wilcox.test(adcd,wtcd,correct = F,exact = F)[["p.value"]]
  
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
  ggsave(paste(getwd(),"/act_BOXPLOT.pdf",sep=""),
         width = 3.5,height = 3.5,units = "in")
}
{adbl<-pdata$Homeostatic[which(pdata$Type=="ADBL")]
  adcd<-pdata$Homeostatic[which(pdata$Type=="ADCD")]
  wtcd<-pdata$Homeostatic[which(pdata$Type=="WTCD")]
  print(mean(adcd))
  print(sd(adcd))
  print(mean(adbl))
  print(sd(adbl))
  t3<-wilcox.test(adbl,wtcd,correct = F,exact = F)[["p.value"]]
  t2<-wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
  t1<-wilcox.test(adcd,wtcd,correct = F,exact = F)[["p.value"]]
  
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
  ggsave(paste(getwd(),"/hom_BOXPLOT.pdf",sep=""),
         width = 3.5,height = 3.5,units = "in")
}
