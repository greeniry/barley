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
    # for (iter in 1:length(summic))
    # {
    #   divdata[,iter]=divdata[,iter]/summic[iter]
    # }
    divdata=t(as.matrix(divdata))
    sid=unique(substr(rownames(divdata),10,16))
    mdata=divdata[1,]
    for (iter in 1:length(sid))
    {
      mdata=rbind(mdata,apply(divdata[which(substr(rownames(divdata),10,16)==sid[iter]),],2,mean))
    }
    mdata=mdata[-1,]
    rownames(mdata)=sid
    annotation_col<-data.frame(Group = factor(substr(sid,1,5),level=c("WT_CD","WT_BL","AD_CD","AD_BL")))
    rownames(annotation_col)<-sid
    annotation_color<-list(Group=c(`WT_CD`="#95B7CF",`WT_BL`="#0570B0",
                                   `AD_CD`="#CB90A4",`AD_BL`="#CE1256"))
    pheatmap(t(mdata[c(16:20,11:15,6:10,1:5),1:10]),scale = "row",show_colnames = T,show_rownames = T,
             color = colorRampPalette(c("#055070", "white","#ae1266"))(50),
             annotation_col = annotation_col,annotation_colors = annotation_color,
             annotation_names_row = TRUE,cluster_cols = F,cluster_rows = T,gaps_col = c(5,10,15),
             filename = paste(getwd(),"/heatmap.pdf",sep=""),
             width=4.5+0.1*nrow(mdata),height = 1+0.15*ncol(mdata))
    # mid=unique(paste(substr(sid,1,2),substr(sid,6,7),sep=""))
    # cdata=mdata[1,]
    # for (iter in 1:length(mid))
    # {
    #   cdata=rbind(cdata,mdata[which(rownames(mdata)==paste(substr(mid[iter],1,2),
    #                                                        "_BL",substr(mid[iter],3,4),sep="")),]-
    #                 mdata[which(rownames(mdata)==paste(substr(mid[iter],1,2),
    #                                                    "_CD",substr(mid[iter],3,4),sep="")),])
    # }
    # cdata=cdata[-1,]
    # rownames(cdata)=mid
    # pheatmap(t(cdata),scale = "row",show_colnames = T,show_rownames = T,
    #          color = colorRampPalette(c("#0570B0","#95B7CF", "white","#CB90A4","#CE1256"))(100),
    #          annotation_names_row = TRUE,cluster_cols = T,cluster_rows = T,
    #          filename = paste("C:/Users/li08/Desktop/barley/16s微生物分析/0311/figures/heatmap/",
    #                           batchname[batch],"_",divname[div],"_BL-CD.pdf",sep=""),
    #          width=2.5+0.1*nrow(mdata),height = 2+0.2*ncol(mdata))
  }
}