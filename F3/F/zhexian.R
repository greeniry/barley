library(ggplot2)
library(stringr)
library(pheatmap)
load(paste(getwd(),"/readin5xfad.RData",sep=""))
ldata=list(data1,data2)
batchname=c("5xFAD_1","5xFAD_2")
rm(data1,data2)
batch = 1
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
    summic=apply(divdata,2,sum)
    
    divdata=divdata[c(8,which(apply(divdata,1,sum)>sum(divdata)*0.005&rownames(divdata)!="Unknown")),]
    for (iter in 1:length(summic))
    {
      divdata[,iter]=divdata[,iter]/summic[iter]
    }
    divdata=t(as.matrix(divdata))
    divdata=divdata[1:50,c(1,7)]
    divdata=log(divdata*1e4+1)
    sid=unique(substr(rownames(divdata),10,16))
    mdata=divdata[1,]
    for (iter in 1:length(sid))
    {
      mdata=rbind(mdata,apply(divdata[which(substr(rownames(divdata),10,16)==sid[iter]),],2,mean))
    }
    mdata=mdata[-1,]
    rownames(mdata)=sid
    semdata=divdata[1,]
    for (iter in 1:length(sid))
    {
      semdata=rbind(semdata,apply(divdata[which(substr(rownames(divdata),10,16)==sid[iter]),],2,sd))/
        sqrt(length(which(substr(rownames(divdata),10,16)==sid[iter])))
    }
    semdata=semdata[-1,]
    rownames(semdata)=sid
    
    bifdata=data.frame(type=substr(rownames(mdata),1,5),
                       month=as.numeric(substr(rownames(mdata),7,7)),
                       mean=mdata[,1],sem=mdata[,1])
    pd <- position_dodge(0.1)
    ggplot(bifdata, aes(x=month, y=mean, colour=type, group=type)) + 
      geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd) +
      geom_line( position=pd) +
      geom_point( size=3, shape=21, position=pd)+theme_classic()
    
    alldata=data.frame(type=substr(rownames(mdata),1,5),
                       month=as.numeric(substr(rownames(mdata),7,7)),
                       mean=mdata[,2],sem=mdata[,2])
    pd <- position_dodge(0.1)
    ggplot(alldata, aes(x=month, y=mean, colour=type, group=type)) + 
      geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd) +
      geom_line( position=pd) +
      geom_point( size=3, shape=21, position=pd)+theme_classic()
  }
}