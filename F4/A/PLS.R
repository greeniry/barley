library(readxl)
library(stringr)
data=read_xlsx(paste(getwd(),"/support/结肠内容物.xlsx",sep=""))
names=unname(unlist(data[,1]))
data=as.matrix(data[,-c(1,2)])
colnames(data)=paste(substr(colnames(data),1,str_length(colnames(data))-31),
                     substr(colnames(data),str_length(colnames(data))-6,str_length(colnames(data))),
                     sep="")
data=data[,c(13:24,31:42)]
# data=log(data)
library(ropls)
library(ggplot2)
oplsdata<-opls(x=t(data),
               y=substr(colnames(data),7,11),
               orthoI = 0,scaleC = c("none", "center", "pareto", "standard")[4])
picdata=data.frame(t1=oplsdata@scoreMN[,1],t2=oplsdata@scoreMN[,2],
                   Group=factor(x=substr(colnames(data),7,11),levels = c("WT_CD","WT_BL","AD_CD","AD_BL")),
                   Type=factor(substr(colnames(data),7,8),
                               level=c("WT","AD")),
                   Feed=factor(substr(colnames(data),10,11),
                               level=c("CD","BL")))
ggplot(picdata,aes(t1,t2,color=Type,shape=Feed)) + 
  geom_point(size=3) + 
  theme_bw() + stat_ellipse(aes(fill=Group),type="norm",geom="polygon",alpha=0.5,color="#333333")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values=c("#0570B0","#CE1256"))+
  scale_fill_manual(values = c("#bbbbbb","#bE0000","#ffff89","#055080"))+
  labs(x=paste("t1(",signif(oplsdata@modelDF[["R2X"]][1]*100,2),"%)",sep=""),
       y=paste("t2(",signif(oplsdata@modelDF[["R2X"]][2]*100,2),"%)",sep=""))
