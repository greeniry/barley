library(ggplot2)
library(stringr)
load(paste(getwd(),"/readin5xfad.RData",sep=""))
ldata=list(data1,data2)
batchname=c("5xFAD_1","5xFAD_2")
rm(data1,data2)
batch = 2
{
  data=ldata[[batch]]
  data$Genus[which(data$Genus!="Unknown")]=
    paste("g_",data$Genus[which(data$Genus!="Unknown")],sep="")
  data$Genus[which(data$Genus=="Unknown"&data$Family!="Unknown")]=
    paste("f_",data$Family[which(data$Genus=="Unknown"&data$Family!="Unknown")],sep="")
  data$Genus[which(data$Genus=="Unknown"&data$Family=="Unknown"&data$Order!="Unknown")]=
    paste("o_",data$Order[which(data$Genus=="Unknown"&data$Family=="Unknown"&data$Order!="Unknown")],sep="")
  data=data[,c(1:7,which(substr(colnames(data),16,16)=="2"))]
  divname=c("Family","Genus")
  div=1
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
    divdata=divdata[which(apply(divdata,1,sum)>sum(divdata)*0.01&rownames(divdata)!="Unknown"),]
    for (iter in 1:length(summic))
    {
      divdata[,iter]=divdata[,iter]/summic[iter]
    }
    divdata=t(as.matrix(divdata))
    sid=unique(substr(rownames(divdata),10,11))
    mdata=divdata[1,]
    for (iter in 1:length(sid))
    {
      mdata=rbind(mdata,apply(divdata[which(substr(rownames(divdata),10,11)==sid[iter]),],2,mean))
    }
    mdata=mdata[-1,]
    rownames(mdata)=sid
    mdata=cbind(mdata,Others=0)
    mdata[,ncol(mdata)]=1-apply(mdata,1,sum)
    # ddata=mdata[,which(apply(mdata[5:8,],2,mean)>0.01)]
    #ddata=cbind(mdata[,7],ddata)
    #colnames(ddata)[1]=colnames(mdata)[7]
    # ddata[,ncol(ddata)]=ddata[,ncol(ddata)]+apply(mdata[,which(apply(mdata[5:8,],2,mean)<=0.01)],1,sum)-mdata[,7]
    picdata<-data.frame(
      type=factor(rep(rownames(mdata),ncol(mdata)),
                  levels=c("WT","AD")),
      value=as.numeric(mdata),
      Mic=factor(rep(colnames(mdata),each=2),levels = c(colnames(mdata)[order(mdata[2,-ncol(mdata)])],"Others")))
    colorset=c(
      "#ffff89","#bbbbbb","#8dd3c7","#41b6b4","#df3b2c",
      "#78a659","#7a0167","#41b6c4","#1c808d",
      "#8c96c6","#bc80cd","#e7298a","#237443","#fa8fb5",
      "#ac4c00","#9ebcca","#eee3a1","#0570a0","#cE1256","#525252"
    )
    colorset2=c(
      "#df3b2c","#78a659","#7a0167","#41b6c4","#4c508d",
      "#8c96c6","#bc80cd","#e7298a","#237443","#fa8fb5",
      "#ac4c00","#9ebcca","#eee3a1","#30627B","#AB3A79","#727272"
    )
    ggplot(picdata, aes(x=type,y=value,fill=Mic)) +
      geom_col(position = 'fill')+theme_classic()+
      scale_fill_manual(values=colorset2[(17-ncol(mdata)):16])
  }
}
library(ropls)
id=unique(data[,div+4])
divdata=data[1,8:ncol(data)]
for (iter in 1:length(id))
{
  divdata=rbind(divdata,apply(data[which(data[,div+4]==id[iter]),8:ncol(data)],2,sum))
}
divdata=divdata[-1,]
rownames(divdata)=id
divdata=t(as.matrix(divdata))
oplsdata<-opls(x=divdata,y=substr(rownames(divdata),10,11),crossvalI = 1,
               orthoI = 0,scaleC = c("none", "center", "pareto", "standard")[4])
picdata=data.frame(t1=oplsdata@scoreMN[,1],t2=oplsdata@scoreMN[,2],
                   Group=factor(x=substr(rownames(divdata),10,11),levels = c("WT","AD")))
# pcs=prcomp(divdata)
# spcs=summary(pcs)
# picdata=data.frame(t1=pcs$x[,1],t2=pcs$x[,2],
#                    Group=factor(x=substr(rownames(divdata),10,11),levels = c("WT","AD")))
ggplot(picdata,aes(t1,t2,shape=Group,col=Group)) + 
  stat_ellipse(aes(fill=Group),geom="polygon",alpha=0.8,color="#333333")+
  geom_point(size=3) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values=c(`WT`="#036EB8",`AD`="#D70051"))+
  scale_fill_manual(values = c("#6c9abb","#d97797","#4d869e","#e065a3"))+
  labs(x= paste("t1(",17,"%)",sep=""),
       y=paste("t2(",11,"%)",sep=""),
       title = paste("PLS analysis",sep=""))

