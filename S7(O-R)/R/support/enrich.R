library(readxl)
info=read_xlsx(paste(getwd(),"/xueqing_info2.xlsx",sep = ""),sheet=1)
clss_info=read_xlsx(paste(getwd(),"/xueqing_info2.xlsx",sep = ""),sheet=2)
library(KEGGREST)
onehot=data.frame(name="",pathway="",pathway_id="")
de_info=info[which(info$de=="T"),]
for (i in 1:nrow(de_info))
{
  if (!is.na(de_info$KEGG[i]))
  {
    t<-keggGet(de_info$KEGG[i])[[1]]$PATHWAY
    if (length(t)>0)
    {
      onehot=rbind(onehot,data.frame(name=de_info$Description[i],pathway=unname(t),
                                     pathway_id=names(t)))
    }
  }
}
onehot=onehot[-1,]
enrichment<-data.frame(
  pathway=onehot$pathway[which(!duplicated(onehot$pathway))],
  id=onehot$pathway_id[which(!duplicated(onehot$pathway))],Hits=0)
for (iter in 1:nrow(enrichment))
{
  temp=onehot$name[which(onehot$pathway==enrichment$pathway[iter])]
  enrichment$Hits[iter]=length(temp)
}
writexl::write_xlsx(paste(getwd(),"/kegg_enrichment.xlsx",sep = ""))
writexl::write_xlsx(paste(getwd(),"/kegg_onehot.xlsx",sep = ""))

enrichment<-cbind(enrichment,superclass="",class="")
for (iter in 1:nrow(enrichment))
{
  # rest<-keggGet(paste("hsa",substr(enrichment$id[i],4,10),sep=""))
  rest<-keggGet(enrichment$id[iter])
  class<-rest[[1]]$CLASS
  if (!is.null(class))
  {
    class<-strsplit(class,"; ")[[1]]
    if (class[1]!=""&(!is.na(class[1])))
    {
      enrichment$superclass[iter]=class[1]
    }
    if (class[2]!=""&(!is.na(class[2])))
    {
      enrichment$class[iter]=class[2]
    }
  }
  # enrichment$total=length(rest[[1]]$COMPOUND)
}
writexl::write_xlsx(enrichment,paste(getwd(),"/kegg_enrichment.xlsx",sep = ""))

onehot2=data.frame(name="",pathway="",pathway_id="")
for (i in 385:nrow(info))
{
  if (!is.na(info$KEGG[i]))
  {
    t<-keggGet(info$KEGG[i])[[1]]$PATHWAY
    if (length(t)>0)
    {
      onehot2=rbind(onehot2,data.frame(name=info$Description[i],pathway=unname(t),
                                       pathway_id=names(t)))
    }
  }
}
onehot2=onehot2[-1,]
writexl::write_xlsx(onehot2,paste(getwd(),"/kegg_onehot2.xlsx",sep = ""))

enrichment<-data.frame(
  enrichment,total=0)
for (iter in 1:nrow(enrichment))
{
  temp=onehot$name[which(onehot2$pathway==enrichment$pathway[iter])]
  enrichment$total[iter]=length(temp)
}
writexl::write_xlsx(enrichment,paste(getwd(),"/kegg_enrichment.xlsx",sep = ""))
enrichment<-data.frame(
  enrichment,ratio=0,pvalue=0,metabolites="")
for (iter in 1:nrow(enrichment))
{
  enrichment$ratio[iter]=enrichment$Hits[iter]/enrichment$total[iter]
  enrichment$pvalue[iter]=fisher.test(matrix(
    c(enrichment$Hits[iter],48-enrichment$Hits[iter],
      enrichment$total[iter]-enrichment$Hits[iter],
      388-48-enrichment$total[iter]+enrichment$Hits[iter]),
    ncol=2))[["p.value"]]
  enrichment$metabolites[iter]=paste(onehot$name[which(onehot$pathway==enrichment$pathway[iter])],collapse=", ")
}
writexl::write_xlsx(enrichment,paste(getwd(),"/kegg_enrichment.xlsx",sep = ""))

data<-read_xlsx(paste(getwd(),"/kegg_enrichment2.xlsx",sep = ""),2)
data$pathway=factor(data$pathway,levels = data$pathway[order(data$Hits,decreasing = T)])
ggplot()+geom_col(data=data,mapping = aes(x=Hits,y=pathway,fill=ratio))+
  theme_classic()+scale_fill_gradient(low="#6bb4d4",high="#7d5094")

data2<-read_xlsx(paste(getwd(),"/kegg_onehot2(1).xlsx",sep = ""),2)
id=unique(data2$pathway)
data2<-data.frame(compound="",pathway="",fc=0,pvalue=0)
for (i in 1:length(id))
{
  cnm<-onehot2$name[which(onehot2$pathway==id[i])]
  ids=0
  for (j in 1:length(which(onehot2$pathway==id[i])))
  {
    ids=c(ids,which(info$Description==cnm[j]))
  }
  ids=ids[-1]
  data2=rbind(data2,data.frame(compound=cnm,pathway=id[i],
                               fc=info$fc[ids],pvalue=info$pvalue[ids]))
}
data2=data2[-1,]
writexl::write_xlsx(data2,paste(getwd(),"/test.xlsx",sep = ""))

data2<-read_xlsx(paste(getwd(),"/test.xlsx",sep = ""))
enrichment<-data.frame(
  pathway=unique(data2$pathway),hit=0,total=0,ratio=0,pvalue=0,metabolites="")
for (iter in 1:nrow(enrichment))
{
  enrichment$hit[iter]=length(which(data2$pathway==enrichment$pathway[iter]&data2$pvalue<0.05))
  enrichment$total[iter]=length(which(data2$pathway==enrichment$pathway[iter]))
  enrichment$ratio[iter]=enrichment$hit[iter]/enrichment$total[iter]
  enrichment$pvalue[iter]=fisher.test(matrix(
    c(enrichment$hit[iter],48-enrichment$hit[iter],
      enrichment$total[iter]-enrichment$hit[iter],
      388-48-enrichment$total[iter]+enrichment$hit[iter]),
    ncol=2))[["p.value"]]
  enrichment$metabolites[iter]=paste(data2$compound[which(data2$pathway==enrichment$pathway[iter])],collapse=", ")
}
enrichment=enrichment[-2,]

writexl::write_xlsx(enrichment,paste(getwd(),"/test_enrichment.xlsx",sep = ""))
