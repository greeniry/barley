library(ggplot2)
library(readxl)
enrichment<-read_xlsx(paste(getwd(),"/support/enrichment.xlsx",sep = ""))
pic_3<-data.frame(class=enrichment$class,hit=enrichment$Hits)
pic_3<-data.frame(class=factor(enrichment$class,levels = pic_3$class[order(pic_3$hit,decreasing = T)]),hit=enrichment$Hits)
pic_3=pic_3[-which(pic_3$hit==0),]
ggplot()+geom_col(data=pic_3,mapping = aes(x=hit,y=class,fill=hit))+
  theme_classic()+scale_fill_gradient(low="#6bb4d4",high="#7d5094")
