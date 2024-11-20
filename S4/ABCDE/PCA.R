library(ggplot2)
library(stringr)
load(paste(getwd(),"/readin5xfad.RData",sep=""))
ldata=list(data1,data2)
batchname=c("5xFAD_1","5xFAD_2")
rm(data1,data2)
batch = 1
{
  data=ldata[[batch]]
  name=data[,1:7]
  data=data[,-c(1:7)]
  data=as.matrix(data)
  for (month in 2:6)
  {
    pdata=data[,which(substr(colnames(data),16,16)==as.character(month))]
    divname=c("Family","Genus")
    div=2
    {
      id=unique(name[,div+4])
      divdata=pdata[1,]
      for (iter in 1:length(id))
      {
        if (length(which(name[,div+4]==id[iter]))>1)
          divdata=rbind(divdata,apply(pdata[which(name[,div+4]==id[iter]),],2,sum))
        else
          divdata=rbind(divdata,pdata[which(name[,div+4]==id[iter]),])
      }
      divdata=divdata[-1,]
      rownames(divdata)=id
      summic=apply(divdata,2,sum)
      divdata=divdata[which(apply(divdata,1,sum)>sum(divdata)*0.005&rownames(divdata)!="Unknown"),]
      for (iter in 1:length(summic))
      {
        divdata[,iter]=divdata[,iter]/summic[iter]
      }
      divdata=t(as.matrix(divdata))
      # oplsdata<-opls(x=divdata,y=substr(colnames(pdata),9,13),crossvalI =1,
      #                orthoI = 0,scaleC = c("none", "center", "pareto", "standard")[4])
      # picdata=data.frame(t1=oplsdata@scoreMN[,1],t2=oplsdata@scoreMN[,2],
      #                    Group=factor(substr(colnames(pdata),9,13),
      #                                 level=c("WT_CD","WT_BL","AD_CD","AD_BL")))
      pcs=prcomp(divdata)
      ddata=t(pdata[which(name$Genus!="Unknown"),])
      for (i in 1:nrow(ddata))
      {
        ddata[i,]=ddata[i,]/sum(ddata[i,])
      }
      pcs=prcomp(ddata)
      spcs=summary(pcs)
      picdata=data.frame(t1=pcs$x[,1],t2=pcs$x[,2],
                         Group=factor(substr(colnames(pdata),10,14),
                                      level=c("WT_CD","WT_BL","AD_CD","AD_BL")),
                         Type=factor(substr(colnames(pdata),10,11),
                                      level=c("WT","AD")),
                         Feed=factor(substr(colnames(pdata),13,14),
                                      level=c("CD","BL")))
      
      ggplot(picdata,aes(t1,t2,shape=Feed,col=Type)) + 
        stat_ellipse(aes(fill=Group),geom="polygon",alpha=0.8,color="#333333")+
        geom_point(size=3) + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values=c(`WT`="#036EB8",`AD`="#D70051"))+
        scale_fill_manual(values = c("#6c9abb","#d97797","#4d869e","#e065a3"))+
        labs(x= paste("PC1(",signif(spcs$importance[2,1]*100,2),"%)",sep=""),
             y=paste("PC2(",signif(spcs$importance[2,2]*100,2),"%)",sep=""),
             title = paste(month," Month",sep=""))

      ggsave(paste(getwd(),"/",
                   month,"_","Month_PCA.pdf",sep=""),
             height = 4,width = 5,units = "in")
    }
    
  }
}