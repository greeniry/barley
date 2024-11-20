library(ggplot2)
load(paste(getwd(),"/support/batchcombined.RData",sep=""))
#butyrate
{
  data=mSet[["dataSet"]][["Combat_edata"]]
  adbl<-data[c(13:19,60:65),5]+data[c(13:19,60:65),6]
  adcd<-data[c(20:21,57:59),5]+data[c(20:21,57:59),6]
  wtbl<-data[c(38:40,54:56),5]+data[c(38:40,54:56),6]
  wtcd<-data[c(41:42,51:53),5]+data[c(41:42,51:53),6]
  
  t1<-wilcox.test(wtbl,wtcd,correct = F,exact = F)[["p.value"]]
  t2<-wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
  
  t3=rcompanion::scheirerRayHare(value~type+feed,data=data.frame(
    type=factor(c(rep("wt",11),rep("ad",18)),levels = c("wt","ad")),
    feed=factor(c(rep("cd",5),rep("bl",6),rep("cd",5),rep("bl",13)),levels = c("cd","bl")),
    group=factor(c(rep("wtcd",5),rep("wtbl",6),rep("adcd",5),rep("adbl",13)),
                 levels = c("wtcd","wtbl","adcd","adbl")),
    value=c(wtcd,wtbl,adcd,adbl)))[2,4]
  
  picdata<-data.frame(
    Group=factor(c(
      rep("WT_CD",length(wtcd)),rep("WT_BL",length(wtbl)),
      rep("AD_CD",length(adcd)),rep("AD_BL",length(adbl))
    ),levels = c("WT_CD","WT_BL","AD_CD","AD_BL")),
    mdata=c(wtcd,wtbl,adcd,adbl))
  color=c(rep("#4d869e",length(wtcd)),rep("#0570B0",length(wtbl)),
          rep("#cc82a5",length(adcd)),rep("#CE1256",length(adbl)))
  
  P <- ggplot(picdata,aes(x=Group,y=mdata))+ 
    stat_boxplot(geom = "errorbar",width=0.15,color=c("#4d869e","#0570B0","#cc82a5","#CE1256"))+ #由于自带的箱形图没有胡须末端没有短横线，使用误差条的方式补上
    geom_boxplot(size=0.7,fill="white",color=c("#4d869e","#0570B0","#cc82a5","#CE1256"))+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
    geom_jitter(width =0.1,shape = 21,size=2,stroke=1,color=color)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
    theme_bw()+ #背景变为白色
    theme(legend.position="none", #不需要图例
          panel.grid.major = element_blank(), #不显示网格线
          panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.05,
                                                   max(unlist(picdata$mdata))*1.17)+
    ylab("log abundance")+xlab("") #设置x轴和y轴的标题
  if (!is.nan(t1))
  {
    P=P+annotate("segment", x = 0.9, xend = 2.1, y = max(unlist(picdata$mdata))*1.04,
                 yend = max(unlist(picdata$mdata))*1.04,size = 1)
    if (t1<0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.05, label="***",size=8)
    if (t1<0.01&t1>=0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.05, label="**",size=8)
    if (t1<=0.05&t1>=0.01)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.05, label="*",size=8)
    if (t1>0.05)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.07, label="ns",size=4)
  }
  if (!is.nan(t2))
  {
    P=P+annotate("segment", x = 2.9, xend = 4.1, y = max(unlist(picdata$mdata))*1.04,
                 yend = max(unlist(picdata$mdata))*1.04,size = 1)
    if (t2<0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.05, label="***",size=8)
    if (t2<0.01&t2>=0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.05, label="**",size=8)
    if (t2<=0.05&t2>=0.01)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.05, label="*",size=8)
    if (t2>0.05)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.07, label="ns",size=4)
  }
  if (!is.nan(t3))
  {
    P=P+annotate("segment", x = 1, xend = 4, y = max(unlist(picdata$mdata))*1.14,
                 yend = max(unlist(picdata$mdata))*1.14,size = 1)
    if (t3<0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.15, label="***",size=8)
    if (t3<0.01&t3>=0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.15, label="**",size=8)
    if (t3<=0.05&t3>=0.01)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.15, label="*",size=8)
    if (t3>0.05)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.17, label="ns",size=4)
  }
}
P
#scfas
{
  data=mSet[["dataSet"]][["Combat_edata"]]
  scfa_index=c(4,5,13,21,27,28,30)+1
  adbl<-unlist(apply(data[c(13:19,60:65),scfa_index],1,sum))
  adcd<-unlist(apply(data[c(20:21,57:59),scfa_index],1,sum))
  wtbl<-unlist(apply(data[c(38:40,54:56),scfa_index],1,sum))
  wtcd<-unlist(apply(data[c(41:42,51:53),scfa_index],1,sum))
  
  t1<-wilcox.test(wtbl,wtcd,correct = F,exact = F)[["p.value"]]
  t2<-wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
  
  t3=rcompanion::scheirerRayHare(value~type+feed,data=data.frame(
    type=factor(c(rep("wt",11),rep("ad",18)),levels = c("wt","ad")),
    feed=factor(c(rep("cd",5),rep("bl",6),rep("cd",5),rep("bl",13)),levels = c("cd","bl")),
    group=factor(c(rep("wtcd",5),rep("wtbl",6),rep("adcd",5),rep("adbl",13)),
                 levels = c("wtcd","wtbl","adcd","adbl")),
    value=c(wtcd,wtbl,adcd,adbl)))[2,4]
  
  picdata<-data.frame(
    Group=factor(c(
      rep("WT_CD",length(wtcd)),rep("WT_BL",length(wtbl)),
      rep("AD_CD",length(adcd)),rep("AD_BL",length(adbl))
    ),levels = c("WT_CD","WT_BL","AD_CD","AD_BL")),
    mdata=c(wtcd,wtbl,adcd,adbl))
  color=c(rep("#4d869e",length(wtcd)),rep("#0570B0",length(wtbl)),
          rep("#cc82a5",length(adcd)),rep("#CE1256",length(adbl)))
  
  P <- ggplot(picdata,aes(x=Group,y=mdata))+ 
    stat_boxplot(geom = "errorbar",width=0.15,color=c("#4d869e","#0570B0","#cc82a5","#CE1256"))+ #由于自带的箱形图没有胡须末端没有短横线，使用误差条的方式补上
    geom_boxplot(size=0.7,fill="white",color=c("#4d869e","#0570B0","#cc82a5","#CE1256"))+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
    geom_jitter(width =0.1,shape = 21,size=2,stroke=1,color=color)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
    theme_bw()+ #背景变为白色
    theme(legend.position="none", #不需要图例
          panel.grid.major = element_blank(), #不显示网格线
          panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.05,
                                                   max(unlist(picdata$mdata))*1.17)+
    ylab("log abundance")+xlab("") #设置x轴和y轴的标题
  if (!is.nan(t1))
  {
    P=P+annotate("segment", x = 0.9, xend = 2.1, y = max(unlist(picdata$mdata))*1.04,
                 yend = max(unlist(picdata$mdata))*1.04,size = 1)
    if (t1<0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.05, label="***",size=8)
    if (t1<0.01&t1>=0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.05, label="**",size=8)
    if (t1<=0.05&t1>=0.01)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.05, label="*",size=8)
    if (t1>0.05)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.07, label="ns",size=4)
  }
  if (!is.nan(t2))
  {
    P=P+annotate("segment", x = 2.9, xend = 4.1, y = max(unlist(picdata$mdata))*1.04,
                 yend = max(unlist(picdata$mdata))*1.04,size = 1)
    if (t2<0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.05, label="***",size=8)
    if (t2<0.01&t2>=0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.05, label="**",size=8)
    if (t2<=0.05&t2>=0.01)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.05, label="*",size=8)
    if (t2>0.05)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.07, label="ns",size=4)
  }
  if (!is.nan(t3))
  {
    P=P+annotate("segment", x = 1, xend = 4, y = max(unlist(picdata$mdata))*1.14,
                 yend = max(unlist(picdata$mdata))*1.14,size = 1)
    if (t3<0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.15, label="***",size=8)
    if (t3<0.01&t3>=0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.15, label="**",size=8)
    if (t3<=0.05&t3>=0.01)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.15, label="*",size=8)
    if (t3>0.05)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.17, label="ns",size=4)
  }
}
P