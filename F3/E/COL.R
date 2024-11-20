library(ggplot2)
library(stringr)
load(paste(getwd(),"/readin5xfad.RData",sep=""))
ldata=list(data1,data2)
batchname=c("5xFAD_1","5xFAD_2")
rm(data1,data2)
batch = 2
{
  data=ldata[[batch]]
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
    divdata=divdata[,which(substr(colnames(divdata),16,16)=="2"|substr(colnames(divdata),16,16)=="6")]
    summic=apply(divdata,2,sum)
    divdata=divdata[which(apply(divdata,1,sum)>sum(divdata)*0.005&rownames(divdata)!="Unknown"),]
    for (iter in 1:length(summic))
    {
      divdata[,iter]=divdata[,iter]/summic[iter]
    }
    divdata=t(as.matrix(divdata))
    sid=unique(substr(rownames(divdata),10,16))
    mdata=divdata[1,]
    for (iter in 1:length(sid))
    {
      mdata=rbind(mdata,apply(divdata[which(substr(rownames(divdata),10,16)==sid[iter]),],2,mean))
    }
    mdata=mdata[-1,]
    rownames(mdata)=sid
    mdata=mdata[,-c(11,13,4)]
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
      Mic=factor(rep(colnames(mdata),each=8),levels = colnames(mdata)[order(mdata[8,]+mdata[2,]+mdata[6,]+mdata[4,])]))
    picdata$value=log(1000*picdata$value)
    picdata$value[which(picdata$value<0)]=0
    picdata$value[which(picdata$month=="2")]=-picdata$value[which(picdata$month=="2")]
    
    ggplot(picdata)+
      geom_bar(aes(x=Mic,y=value,group=group,fill=group,alpha=month),stat = 'identity',width = 0.5,
               position = position_dodge(width = 0.5))+
      scale_fill_manual(values = c(
        "WT_CD"="#69b8d9","WT_BL"="#55889d",
        "AD_CD"="#ed7ba1","AD_BL"="#d66887"
      ))+scale_alpha_manual(values=c("2"=0.75,"6"=1))+
      coord_flip()+theme_bw()
    
  }
}
