load(paste(getwd(),"/support/jiechang.RData",sep=""))
library(readxl)
library(ggplot2)
scfainfo=read_xlsx(paste(getwd(),"/support/class.xlsx",sep=""),sheet=3)
id=0
for (i in 1:18)
{
  id=c(id,which(info$Description==scfainfo$Description[i]))
}
id=id[-1]
data=log(data)
wtcd=apply(data[id,37:42],2,sum)
wtbl=apply(data[id,31:36],2,sum)
adcd=apply(data[id,19:24],2,sum)
adbl=apply(data[id,13:18],2,sum)
print(mean(adbl))
print(sd(adbl))
print(mean(adcd))
print(sd(adcd))
# wtcd=log(wtcd)
# wtbl=log(wtbl)
# adcd=log(adcd)
# adbl=log(adbl)
{
  t1=wilcox.test(wtbl,wtcd,correct = F,exact = F)[["p.value"]]
  t2=wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
  t3<-rcompanion::scheirerRayHare(value~type+feed,data=data.frame(
    type=factor(c(rep("wt",12),rep("ad",12)),levels = c("wt","ad")),
    feed=factor(c(rep("cd",6),rep("bl",6),rep("cd",6),rep("bl",6)),levels = c("cd","bl")),
    group=factor(c(rep("wtcd",6),rep("wtbl",6),rep("adcd",6),rep("adbl",6)),
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
          panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.005,
                                                   max(unlist(picdata$mdata))*1.04)+
    ylab("Relative abundance(%)")+xlab("") #设置x轴和y轴的标题
  if (!is.nan(t1))
  {
    P=P+annotate("segment", x = 0.9, xend = 2.1, y = max(unlist(picdata$mdata))*1.01,
                 yend = max(unlist(picdata$mdata))*1.01,size = 1)
    if (t1<0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.012, label="***",size=8)
    if (t1<0.01&t1>=0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.012, label="**",size=8)
    if (t1<=0.05&t1>=0.01)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.012, label="*",size=8)
    if (t1>0.05)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.015, label="ns",size=4)
  }
  if (!is.nan(t2))
  {
    P=P+annotate("segment", x = 2.9, xend = 4.1, y = max(unlist(picdata$mdata))*1.01,
                 yend = max(unlist(picdata$mdata))*1.01,size = 1)
    if (t2<0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.012, label="***",size=8)
    if (t2<0.01&t2>=0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.012, label="**",size=8)
    if (t2<=0.05&t2>=0.01)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.012, label="*",size=8)
    if (t2>0.05)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.015, label="ns",size=4)
  }
  if (!is.nan(t3))
  {
    P=P+annotate("segment", x = 1, xend = 4, y = max(unlist(picdata$mdata))*1.03,
                 yend = max(unlist(picdata$mdata))*1.03,size = 1)
    if (t3<0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.032, label="***",size=8)
    if (t3<0.01&t3>=0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.032, label="**",size=8)
    if (t3<=0.05&t3>=0.01)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.032, label="*",size=8)
    if (t3>0.05)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.035, label="ns",size=4)
  }
}
P


scfainfo=scfainfo[which(scfainfo$...2==c("丁酸","丙酸","戊酸","乙酸")[1]),]
id=0
for (i in 1:18)
{
  id=c(id,which(info$Description==scfainfo$Description[i]))
}
id=id[-1]
wtcd=apply(data[id,37:42],2,sum)
wtbl=apply(data[id,31:36],2,sum)
adcd=apply(data[id,19:24],2,sum)
adbl=apply(data[id,13:18],2,sum)
print(mean(adbl))
print(sd(adbl))
print(mean(adcd))
print(sd(adcd))
# wtcd=log(wtcd)
# wtbl=log(wtbl)
# adcd=log(adcd)
# adbl=log(adbl)
{
  t1=wilcox.test(wtbl,wtcd,correct = F,exact = F)[["p.value"]]
  t2=wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
  t3=rcompanion::scheirerRayHare(value~type+feed,data=data.frame(
    type=factor(c(rep("wt",6),rep("ad",12)),levels = c("wt","ad")),
    feed=factor(c(rep("cd",3),rep("bl",3),rep("cd",6),rep("bl",6)),levels = c("cd","bl")),
    group=factor(c(rep("wtcd",3),rep("wtbl",3),rep("adcd",6),rep("adbl",6)),
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
          panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.005,
                                                   max(unlist(picdata$mdata))*1.04)+
    ylab("Relative abundance(%)")+xlab("") #设置x轴和y轴的标题
  if (!is.nan(t1))
  {
    P=P+annotate("segment", x = 0.9, xend = 2.1, y = max(unlist(picdata$mdata))*1.01,
                 yend = max(unlist(picdata$mdata))*1.01,size = 1)
    if (t1<0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.012, label="***",size=8)
    if (t1<0.01&t1>=0.001)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.012, label="**",size=8)
    if (t1<=0.05&t1>=0.01)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.012, label="*",size=8)
    if (t1>0.05)
      P=P+annotate("text", x = 1.5, y = max(unlist(picdata$mdata))*1.015, label="ns",size=4)
  }
  if (!is.nan(t2))
  {
    P=P+annotate("segment", x = 2.9, xend = 4.1, y = max(unlist(picdata$mdata))*1.01,
                 yend = max(unlist(picdata$mdata))*1.01,size = 1)
    if (t2<0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.012, label="***",size=8)
    if (t2<0.01&t2>=0.001)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.012, label="**",size=8)
    if (t2<=0.05&t2>=0.01)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.012, label="*",size=8)
    if (t2>0.05)
      P=P+annotate("text", x = 3.5, y = max(unlist(picdata$mdata))*1.015, label="ns",size=4)
  }
  if (!is.nan(t3))
  {
    P=P+annotate("segment", x = 1, xend = 4, y = max(unlist(picdata$mdata))*1.03,
                 yend = max(unlist(picdata$mdata))*1.03,size = 1)
    if (t3<0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.032, label="***",size=8)
    if (t3<0.01&t3>=0.001)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.032, label="**",size=8)
    if (t3<=0.05&t3>=0.01)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.032, label="*",size=8)
    if (t3>0.05)
      P=P+annotate("text", x = 2.5, y = max(unlist(picdata$mdata))*1.035, label="ns",size=4)
  }
}
P