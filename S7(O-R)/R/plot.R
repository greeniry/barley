library(readxl)
library(ggplot2)
enrichment<-readxl::read_xlsx(paste(getwd(),"/support/test_enrichment.xlsx",sep=""))

id=which(enrichment$pvalue<0.05)
alldata=data.frame(
  ID=factor(x=enrichment$pathway[id],
            levels = (enrichment$pathway[id])[order(enrichment$pvalue[id],decreasing = T)]),
  Ratio=enrichment$ratio[id],
  `-Log10(P.value)`=-log10(enrichment$pvalue[id]),
  Hits=enrichment$hit[id]
)
ggplot()+geom_col(data=alldata,aes(x=Hits,y=ID,
                                   fill=X.Log10.P.value.))+
  scale_fill_continuous(high = "#CE1256",low = "#CB90A4")+
  theme_classic()+
  labs(x="Hits",y="")
