library(readxl)
data<-read_xlsx(paste(getwd(),"/血清_all.xlsx",sep=""),sheet = 1)
info<-read.csv(paste(getwd(),"/name_map血清.csv",sep=""))
data<-cbind(data[,1:2],info[,2:5],data[,3:51])
info<-data[,1:8]
data<-as.matrix(data[,-c(1:8)])
{
  library(ropls)
  library(ggplot2)
  oplsdata<-opls(x=t(data[,c(42:47,36:41,24:29,18:23)]),
                 y=rep(c("WT_CD","WT_BL","AD_CD","AD_BL"),each=6),
                 orthoI = 0,scaleC = c("none", "center", "pareto", "standard")[4])
  picdata=data.frame(t1=oplsdata@scoreMN[,1],t2=oplsdata@scoreMN[,2],
                     Group=factor(rep(c("WT_CD","WT_BL","AD_CD","AD_BL"),each=6),
                                  level=c("WT_CD","WT_BL","AD_CD","AD_BL")),
                     type=factor(rep(c("CD","BL","CD","BL"),each=6),
                                 level=c("CD","BL")),
                     Month=factor(rep(c("WT","AD"),each=12),
                                  level=c("WT","AD")))
  ggplot(picdata,aes(t1,t2,shape=type,color=Month)) + 
    geom_point(size=3) + 
    theme_bw() + stat_ellipse(aes(fill=Group),type="norm",geom="polygon",alpha=0.5,color="#333333")+
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(values=c("#0570B0","#CE1256"))+
    scale_fill_manual(values = c("#bbbbbb","#bE0000","#ffff89","#055080"))+
    labs(x=paste("t1(",signif(oplsdata@modelDF[["R2X"]][1]*100,2),"%)",sep=""),
         y=paste("t2(",signif(oplsdata@modelDF[["R2X"]][2]*100,2),"%)",sep=""))
}