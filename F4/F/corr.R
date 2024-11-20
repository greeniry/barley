library(readxl)
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
jiechangscfa<-rbind(apply(data[id,c(37:42,31:36,19:24,13:18)],2,sum),
                    apply(data[id2,c(37:42,31:36,19:24,13:18)],2,sum),
                    data[id[c(2,1,4,15,7)],c(37:42,31:36,19:24,13:18)])
rownames(jiechangscfa)=c("SCFA_ALL","Butanoic-Base acid",scfainfo$Description[c(2,1,4,15,7)])

load("C:/Users/li08/Desktop/barley/数据整合/codetemp/readindata.RData")

Microorganism_baifenbi_5xFAD=Microorganism_baifenbi_5xFAD[1:19,]
#Microorganism_baifenbi_5xFAD=Microorganism_baifenbi_5xFAD[-5,]
for (i in 1:ncol(Microorganism_baifenbi_5xFAD))
{
  Microorganism_baifenbi_5xFAD[,i]=as.numeric(Microorganism_baifenbi_5xFAD[,i])
}
jiechangscfa=log(jiechangscfa)
data=cbind(Microorganism_baifenbi_5xFAD,t(jiechangscfa)[-c(12,16,17,18,24),])
data=as.matrix(data)
corr<-Hmisc::rcorr(data,type="spearman")
corr$P[is.na(corr$P)]=1
rdata=corr$r[1:7,8:14]
pdata=corr$P[1:7,8:14]
# id=which((pdata[1,]<0.05)|(pdata[2,]<0.05)|(pdata[3,]<0.05)|(pdata[4,]<0.05)|
#            (pdata[5,]<0.05)|(pdata[6,]<0.05)|(pdata[7,]<0.05))
# id=which(rdata[2,]>=0)
# id=id[order(colnames(rdata)[id])]
# rdata=rdata[,id]
# pdata=pdata[,id]
corrplot::corrplot(rdata[,],method = "color",
                   type="full",p.mat = pdata[,],sig.level = 0.05,
                   insig = "label_sig",tl.col = "black",cl.pos = 'r',tl.srt=45,
                   col = colorRampPalette(c("#055070", "white","#CB6481"))(50))
rdata=cbind(rdata,c(1,-1))
library(pheatmap)
pheatmap(rdata[,c(1:5,7,6,8)],color=colorRampPalette(c("#055070", "white","#CB6481"))(50),
         cluster_rows = F,cluster_cols = F)

