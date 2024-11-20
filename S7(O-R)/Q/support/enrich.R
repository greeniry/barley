library(readxl)
info=read_xlsx(paste(getwd(),"/xueqing_info2.xlsx",sep = ""),sheet=1)
clss_info=read_xlsx(paste(getwd(),"/xueqing_info2.xlsx",sep = ""),sheet=2)

clss_info$...2[61:91]="Others"
onehot<-data.frame(name="",class="",fc=0,pvalue=0,de="F")
for (i in 1:nrow(info))
{
  if (!is.na(info$subclass[i]))
  {
    onehot<-rbind(onehot,data.frame(name=info$Description[i],
                                    class=clss_info$...2[which(clss_info$subclass==info$subclass[i])],
                                    fc=info$fc[i],pvalue=info$pvalue[i],de=info$de[i]))
  }
}
onehot=onehot[-1,]

writexl::write_xlsx(onehot,paste(getwd(),"/class_onehot.xlsx",sep = ""))

enrichment<-data.frame(
  class=unique(onehot$class),Total=0,Hits=0,Hit_name="",Ratio=0,P.value=0,
  Up_Hits=0,Up_Hit_name="",Up_Ratio=0,Up_P.value=0,
  Down_Hits=0,Down_Hit_name="",Down_Ratio=0,Down_P.value=0)
enrichment=enrichment[-2,]
decount=length(which(onehot$de[which(!duplicated(onehot$name))]=="T"))
upcount=length(which(onehot$de[which(!duplicated(onehot$name))]=="T"&
                       onehot$fc[which(!duplicated(onehot$name))]>2))
downcount=length(which(onehot$de[which(!duplicated(onehot$name))]=="T"&
                         onehot$fc[which(!duplicated(onehot$name))]<1/2))
for (iter in 1:nrow(enrichment))
{
  temp=onehot$name[which(onehot$class==enrichment$class[iter])]
  DE<-onehot$name[which(onehot$class==enrichment$class[iter]&onehot$de=="T")]
  Up<-onehot$name[which(onehot$class==enrichment$class[iter]&onehot$de=="T"&onehot$fc>2)]
  Down<-onehot$name[which(onehot$class==enrichment$class[iter]&onehot$de=="T"&onehot$fc<1/2)]
  enrichment$Total[iter]=length(temp)
  enrichment$Hits[iter]=length(DE)
  enrichment$Up_Hits[iter]=length(Up)
  enrichment$Down_Hits[iter]=length(Down)
  enrichment$Hit_name[iter]=paste(DE,collapse=", ")
  enrichment$Up_Hit_name[iter]=paste(Up,collapse=", ")
  enrichment$Down_Hit_name[iter]=paste(Down,collapse=", ")
  
  enrichment$Ratio[iter]=enrichment$Hits[iter]/enrichment$Total[iter]
  enrichment$P.value[iter]=fisher.test(matrix(
    c(enrichment$Hits[iter],decount-enrichment$Hits[iter],
      enrichment$Total[iter]-enrichment$Hits[iter],
      424-decount-enrichment$Total[iter]+enrichment$Hits[iter]),
    ncol=2))[["p.value"]]
  enrichment$Up_Ratio[iter]=enrichment$Up_Hits[iter]/enrichment$Total[iter]
  enrichment$Up_P.value[iter]=fisher.test(matrix(
    c(enrichment$Up_Hits[iter],upcount-enrichment$Up_Hits[iter],
      enrichment$Total[iter]-enrichment$Up_Hits[iter],
      424-upcount-enrichment$Total[iter]+enrichment$Up_Hits[iter]),
    ncol=2))[["p.value"]]
  enrichment$Down_Ratio[iter]=enrichment$Down_Hits[iter]/enrichment$Total[iter]
  enrichment$Down_P.value[iter]=fisher.test(matrix(
    c(enrichment$Down_Hits[iter],downcount-enrichment$Down_Hits[iter],
      enrichment$Total[iter]-enrichment$Down_Hits[iter],
      424-downcount-enrichment$Total[iter]+enrichment$Down_Hits[iter]),
    ncol=2))[["p.value"]]
}
writexl::write_xlsx(enrichment,paste(getwd(),"/enrichment.xlsx",sep = ""))