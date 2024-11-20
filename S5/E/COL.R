library(ggplot2)
library(stringr)
load(paste(getwd(),"/readin.RData",sep=""))
ldata=list(data1,data2,data3,data4)
batchname=c("3xTg_1","3xTg_2","3xTg_1+2","3xTg_3+1(cd)")
rm(data1,data2,data3,data4)
batch=4
{
  data=ldata[[batch]]
  data$Genus[which(data$Family=="S24-7")]="Muribaculaceae"
  data$Family[which(data$Family=="S24-7")]="Muribaculaceae"
  divname=c("Family","Genus")
  div=2
  {
    id=unique(data[,div+4])
    divdata=data[1,8:ncol(data)]
    for (iter in 1:length(id))
    {
      divdata=rbind(divdata,apply(data[which(data[,div+4]==id[iter]),8:ncol(data)],2,sum))
    }
    divdata=divdata[-1,]
    rownames(divdata)=id
    divdata=divdata[,which(substr(colnames(divdata),15,15)=="2"|substr(colnames(divdata),15,15)=="6")]
    summic=apply(divdata,2,sum)
    divdata=divdata[which(apply(divdata,1,sum)>sum(divdata)*0.005&rownames(divdata)!="Unknown"),]
    for (iter in 1:length(summic))
    {
      divdata[,iter]=divdata[,iter]/summic[iter]
    }
    divdata=t(as.matrix(divdata))
    sid=unique(substr(rownames(divdata),9,15))
    mdata=divdata[1,]
    for (iter in 1:length(sid))
    {
      mdata=rbind(mdata,apply(divdata[which(substr(rownames(divdata),9,15)==sid[iter]),],2,mean))
    }
    mdata=mdata[-1,]
    rownames(mdata)=sid
    mdata=mdata[,-c(11,1,5)]
    picdata<-data.frame(
      # type=factor(rep(substr(rownames(mdata),1,2),ncol(mdata)),
      #             levels=c("WT","AD")),
      # feed=factor(rep(substr(rownames(mdata),4,7),ncol(mdata)),
      #             levels=c("CD_2","CD_6","BL_2","BL_6")),
      group=factor(rep(substr(rownames(mdata),1,5),ncol(mdata)),
                   levels=c("AD_BL","AD_CD","WT_BL","WT_CD")),
      month=factor(rep(substr(rownames(mdata),7,7),ncol(mdata)),
                   levels=c("6","2")),
      value=as.numeric(mdata),
      Mic=factor(rep(colnames(mdata),each=8),levels = colnames(mdata)[order(mdata[8,]+mdata[5,]+mdata[6,]+mdata[7,])]))
    picdata$value=log(1000*picdata$value)
    picdata$value[which(picdata$value<0)]=0
    picdata$value[which(picdata$month=="2")]=-picdata$value[which(picdata$month=="2")]
    
    ggplot(picdata)+
      geom_bar(aes(x=Mic,y=value,group=group,fill=group,alpha=month),stat = 'identity',width = 0.5,
               position = position_dodge(width = 0.5))+
      scale_fill_manual(values = c(
        "WT_CD"="#69b8d9","WT_BL"="#55889d",
        "AD_CD"="#f5a99e","AD_BL"="#e86d7b"
      ))+scale_alpha_manual(values=c("2"=0.75,"6"=1))+
      coord_flip()+theme_bw()
    
  }
}
