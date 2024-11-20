library(ggplot2)
library(stringr)
library(SpiecEasi)
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
    divdata=divdata[which(apply(divdata,1,sum)>sum(divdata)*0.01&rownames(divdata)!="Unknown"),]
    # for (iter in 1:length(summic))
    # {
    #   divdata[,iter]=divdata[,iter]/summic[iter]
    # }
    divdata=t(as.matrix(divdata))
    sid=unique(substr(rownames(divdata),10,16))
    # divdata=divdata[,-c(4,11,13)]
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
    p1<-corrplot::corrplot(cval,method = "color",type="lower",order="AOE",p.mat = pval,sig.level = 0.05,
                           insig = "label_sig",tl.pos = 'ld',tl.col = "black",cl.pos = 'r',
                           col =colorRampPalette(c("#055070", "white","#ae1266"))(50))

    
    
    # 
    # se.mb.amgut <- spiec.easi(divdata, method='mb', lambda.min.ratio=1e-2,
    #                           nlambda=20, pulsar.params=list(rep.num=10))
    # 
    # se.gl.amgut <- spiec.easi(divdata, method='glasso', lambda.min.ratio=1e-2,
    #                           nlambda=20, pulsar.params=list(rep.num=10))
    # 
    # ig.mb     <- adj2igraph(getRefit(se.mb.amgut))
    # ig.gl     <- adj2igraph(getRefit(se.gl.amgut))
    # 
    # library(igraph)
    # ## set size of vertex proportional to clr-mean
    # vsize    <- 15
    # am.coord <- layout.fruchterman.reingold(ig.mb)
    # 
    # plot(ig.mb, layout=am.coord, vertex.size=vsize, vertex.label=NA, main="MB")
    # plot(ig.gl, layout=am.coord, vertex.size=vsize, vertex.label=NA, main="glasso")
    
    # divdata=divdata[,which(apply(divdata[which(substr(rownames(divdata),10,16)=="AD_BL_6"),],2,mean)>0.005)]
    # corr<-Hmisc::rcorr(divdata,type="spearman")
    # corr$P[is.na(corr$P)]=1
    # pdf(file = paste("C:/Users/li08/Desktop/barley/F4/F4_5XFAD/F4_I/","corr.pdf",sep=""),
    #     width = 2+0.5*ncol(divdata),height = 2+0.5*ncol(divdata))
    # p1<-corrplot::corrplot(corr$r,method = "color",type="lower",order="AOE",p.mat = corr$P,sig.level = 0.05,
    #                    insig = "label_sig",tl.pos = 'ld',tl.col = "black",cl.pos = 'r',
    #                    col =colorRampPalette(c("#055070", "white","#ae1266"))(50))
    # dev.off()
  }
  
}