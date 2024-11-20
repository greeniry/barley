library(ggplot2)
library(stringr)
library(SpiecEasi)
load(paste(getwd(),"/readin.RData",sep=""))
ldata=list(data1,data2,data3,data4)
rm(data1,data2,data3,data4)
batchname=c("3xTg_1","3xTg_2","3xTg_1+2","3xTg_3+1(cd)")
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
    divdata=divdata[which(apply(divdata,1,sum)>sum(divdata)*0.01&rownames(divdata)!="Unknown"),]
    # for (iter in 1:length(summic))
    # {
    #   divdata[,iter]=divdata[,iter]/summic[iter]
    # }
    divdata=t(as.matrix(divdata))
    library(SpiecEasi)
    sparcc.data <- sparccboot(divdata,R=500)
    p<-pval.sparccboot(sparcc.data)
    # divdata=divdata[which(substr(rownames(divdata),16,16)=="6"),]
    # p<-sparccboot(divdata,R=1000)
    # p<-pval.sparccboot(p)
    pval=matrix(1,ncol=ncol(divdata),nrow=ncol(divdata))
    cval=pval
    time=1
    for (i in 1:(ncol(divdata)-1))
    {
      for (j in (i+1):ncol(divdata))
      {
        pval[i,j]=p$pvals[time]
        pval[j,i]=p$pvals[time]
        cval[i,j]=p$cors[time]
        cval[j,i]=p$cors[time]
        time=time+1
      }
    }
    colnames(pval)=colnames(divdata)
    rownames(pval)=colnames(divdata)
    colnames(cval)=colnames(divdata)
    rownames(cval)=colnames(divdata)
    # 
    # colnames(sparcc.data[["Cor"]])=colnames(divdata)
    # rownames(sparcc.data[["Cor"]])=colnames(divdata)
    p1<-corrplot::corrplot(cval,method = "color",type="lower",order="hclust",p.mat = pval,sig.level = 0.05,
                           insig = "label_sig",tl.pos = 'ld',tl.col = "black",cl.pos = 'r',
                           col =colorRampPalette(c("#055070", "white","#ae1266"))(50))
    
    
    
    # 
    # sid=unique(substr(rownames(divdata),9,15))
    # divdata2=divdata[,which(apply(divdata[30:67,],2,mean)>0.01)]
    # divdata2=cbind(divdata[,7],divdata2)
    # divdata=divdata2
    # rm(divdata2)
    # colnames(divdata)[1]="Bifidobacterium"
    # corr<-Hmisc::rcorr(divdata,type="spearman")
    # corr$P[is.na(corr$P)]=1
    # pdf(file = paste("C:/Users/li08/Desktop/barley/F4/F4_I/","corr2.pdf",sep=""),
    #     width = 2+0.5*ncol(divdata),height = 2+0.5*ncol(divdata))
    # p1<-corrplot::corrplot(corr$r,method = "color",type="lower",order="AOE",p.mat = corr$P,sig.level = 0.05,
    #                    insig = "label_sig",tl.pos = 'ld',tl.col = "black",cl.pos = 'r',
    #                    col = colorRampPalette(c("#055070", "white","#ae1266"))(50))
    # dev.off()
  }
  
}