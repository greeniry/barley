library(readxl)
library(ggplot2)
load(paste(getwd(),"/support/jiechang.RData",sep=""))
scfainfo=read_xlsx(paste(getwd(),"/support/class.xlsx",sep=""),sheet=3)
id=0
for (i in 1:18)
{
  id=c(id,which(info$Description==scfainfo$Description[i]))
}
id=id[-1]
scfainfo2=scfainfo[which(scfainfo$...2==c("丁酸","丙酸","戊酸","乙酸")[1]),]
id2=0
for (i in 1:18)
{
  id2=c(id2,which(info$Description==scfainfo2$Description[i]))
}
id2=id2[-1]
jiechangscfa<-rbind(apply(data[id,c(28:30,25:27,7:12,1:6)],2,sum),
                    apply(data[id2,c(28:30,25:27,7:12,1:6)],2,sum),
                    data[id[c(1:5,8,10,11,13,18)],c(28:30,25:27,7:12,1:6)])
rownames(jiechangscfa)=c("SCFA_ALL","Butanoic-Base acid",scfainfo$Description[c(1:5,8,10,11,13,18)])
load(paste(getwd(),"/support/readindata.RData",sep=""))

Microorganism_baifenbi_3xTg=Microorganism_baifenbi_3xTg[1:19,]
#Microorganism_baifenbi_5xFAD=Microorganism_baifenbi_5xFAD[-5,]
for (i in 1:ncol(Microorganism_baifenbi_3xTg))
{
  Microorganism_baifenbi_3xTg[,i]=as.numeric(Microorganism_baifenbi_3xTg[,i])
}
jiechangscfa=log(jiechangscfa)
data=cbind(Microorganism_baifenbi_3xTg[-5,],t(jiechangscfa))
data=as.matrix(data)
corr<-Hmisc::rcorr(data,type="spearman")
corr$P[is.na(corr$P)]=1
rdata=corr$r[1:7,8:19]
pdata=corr$P[1:7,8:19]
# id=which((pdata[1,]<0.05)|(pdata[2,]<0.05)|(pdata[3,]<0.05)|(pdata[4,]<0.05)|
#            (pdata[5,]<0.05)|(pdata[6,]<0.05)|(pdata[7,]<0.05))
id=which(rdata[2,]>=0)
id=id[order(colnames(rdata)[id])]
rdata=rdata[,id]
pdata=pdata[,id]
corrplot::corrplot(rdata[,c(9,3,6,7,8,10,1,2,4,5)],method = "color",
                   type="full",p.mat = pdata[,c(9,3,6,7,8,10,1,2,4,5)],sig.level = 0.05,
                   insig = "label_sig",tl.col = "black",cl.pos = 'r',tl.srt=45,
                   col = colorRampPalette(c("#055070", "white","#CB6481"))(50))
rdata=cbind(rdata,c(1,-1))
library(pheatmap)
pheatmap(rdata[,c(9,3,6,7,8,10,1,2,4,5,11)],color=colorRampPalette(c("#055070", "white","#CB6481"))(50),
         cluster_rows = F,cluster_cols = F)

