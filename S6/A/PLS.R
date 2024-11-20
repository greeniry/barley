library(readxl)
library(stringr)
load(paste(getwd(),"/support/jiechang.RData",sep=""))
data=data[,c(1:12,25:30)]
data=log(data)
library(ropls)
library(ggplot2)
oplsdata<-opls(x=t(data),
               y=substr(colnames(data),30,34),
               orthoI = 0,scaleC = c("none", "center", "pareto", "standard")[4])
picdata=data.frame(t1=oplsdata@scoreMN[,1],t2=oplsdata@scoreMN[,2],
                   Group=factor(x=substr(colnames(data),30,34),levels = c("WT_CD","WT_BL","AD_CD","AD_BL")),
                   Type=factor(substr(colnames(data),30,31),
                               level=c("WT","AD")),
                   Feed=factor(substr(colnames(data),33,34),
                               level=c("CD","BL")))
picdata=rbind(picdata,data.frame(t1=c(mean(picdata$t1[13:15]),mean(picdata$t1[16:18])),
                                 t2=c(mean(picdata$t2[13:15]),mean(picdata$t2[16:18])),
                                 Group=factor(x=c("WT_BL","WT_CD"),levels = c("WT_CD","WT_BL","AD_CD","AD_BL")),
                                 Type=factor(x=c("WT","WT"),level=c("WT","AD")),
                                 Feed=factor(x=c("BL","CD"),level=c("CD","BL"))))
ggplot(picdata,aes(t1,t2,color=Type,shape=Feed)) + 
  geom_point(size=3) + 
  theme_bw() + stat_ellipse(aes(fill=Group),type="norm",geom="polygon",alpha=0.5,color="#333333")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values=c("#0570B0","#CE1256"))+
  scale_fill_manual(values = c("#bbbbbb","#bE0000","#ffff89","#055080"))+
  labs(x=paste("t1(",signif(oplsdata@modelDF[["R2X"]][1]*100,2),"%)",sep=""),
       y=paste("t2(",signif(oplsdata@modelDF[["R2X"]][2]*100,2),"%)",sep=""))
