library(ape)
library(vegan)
library(readxl)
library(ggplot2)
library(stringr)
load(paste(getwd(),"/readin5xfad.RData",sep=""))
ldata=list(data1,data2)
batchname=c("5xFAD_1","5xFAD_2")
rm(data1,data2)
batch = 2
{
  data=ldata[[batch]]
  data=cbind(data[,1:7],data[,which(substr(colnames(data),16,16)=="6")])
  pdata=data.frame(
    `Bifidobacterium`=apply(data[which(data$Genus=="Bifidobacterium"),8:ncol(data)],2,sum)/
      apply(data[,8:ncol(data)],2,sum),
    `Allobaculum`=apply(data[which(data$Genus=="Allobaculum"),8:ncol(data)],2,sum)/
      apply(data[,8:ncol(data)],2,sum)
  )
  
  id=substr(rownames(pdata),10,14)
  for (i in 1:2)
  {
    adbl<-unlist(pdata[which(id=="AD_BL"),i])
    adcd<-unlist(pdata[which(id=="AD_CD"),i])
    wtbl<-unlist(pdata[which(id=="WT_BL"),i])
    wtcd<-unlist(pdata[which(id=="WT_CD"),i])
    print(mean(adbl)*100)
    print(sd(adbl)*100)
    print(mean(adcd)*100)
    print(sd(adcd)*100)
    # t1<-t.test(wtbl,wtcd)[["p.value"]]
    # t2<-t.test(adbl,adcd)[["p.value"]]
    # t3<-t.test(adbl,wtcd)[["p.value"]]
    
    t1<-wilcox.test(wtbl,wtcd,correct = F,exact = F)[["p.value"]]
    t2<-wilcox.test(adbl,adcd,correct = F,exact = F)[["p.value"]]
    t3<-rcompanion::scheirerRayHare(value~type+feed,data=data.frame(
      type=factor(c(rep("wt",11),rep("ad",8)),levels = c("wt","ad")),
      feed=factor(c(rep("cd",6),rep("bl",5),rep("cd",3),rep("bl",5)),levels = c("cd","bl")),
      group=factor(c(rep("wtcd",6),rep("wtbl",5),rep("adcd",3),rep("adbl",5)),
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
    picdata$mdata=picdata$mdata*100
    P <- ggplot(picdata,aes(x=Group,y=mdata))+ 
      stat_boxplot(geom = "errorbar",width=0.15,color=c("#4d869e","#0570B0","#cc82a5","#CE1256"))+ #由于自带的箱形图没有胡须末端没有短横线，使用误差条的方式补上
      geom_boxplot(size=0.7,fill="white",color=c("#4d869e","#0570B0","#cc82a5","#CE1256"))+ #size设置箱线图的边框线和胡须的线宽度，fill设置填充颜色，outlier.fill和outlier.color设置异常点的属性
      geom_jitter(width =0.1,shape = 21,size=2,stroke=1,color=color)+ #设置为向水平方向抖动的散点图，width指定了向水平方向抖动，不改变纵轴的值
      ggtitle(colnames(pdata)[i])+ #设置总的标题
      theme_bw()+ #背景变为白色
      theme(legend.position="none", #不需要图例
            panel.grid.major = element_blank(), #不显示网格线
            panel.grid.minor = element_blank())+ylim(min(unlist(picdata$mdata))-max(unlist(picdata$mdata))*0.05,
                                                     max(unlist(picdata$mdata))*1.17)+
      ylab("Relative abundance(%)")+xlab("") #设置x轴和y轴的标题
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
    ggsave(paste(getwd(),"/",
                 colnames(pdata)[i],".pdf",sep=""),
           width = 3,height = 3,units = "in")
  }
}