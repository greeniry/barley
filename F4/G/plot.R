library(ggplot2)
library(readxl)
enrichment<-read_xlsx(paste(getwd(),"/support/KEGG-class-DEM.xlsx",sep=""),sheet=2)
id=which(enrichment$P.value<0.05)
id=id[-8]
alldata=data.frame(
  ID=factor(x=enrichment$class[id],
            levels = (enrichment$class[id])[order(enrichment$P.value[id],decreasing = T)]),
  Ratio=enrichment$Ratio[id],
  `-Log10(P.value)`=-log10(enrichment$P.value[id]),
  Hits=enrichment$Hits[id]
)

ggplot()+geom_col(data=alldata,aes(x=Hits,y=ID,
                                   fill=X.Log10.P.value.))+
  scale_fill_continuous(high = "#CB6481",low = "#fb90b4")+
  theme_classic()+
  labs(x="Hits",y="")
