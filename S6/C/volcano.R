library(ggplot2)
library(readxl)
onehot<-read_xlsx(paste(getwd(),"/support/class_onehot.xlsx",sep=""))
deFA<-onehot[which(onehot$class=="Fatty acids"&onehot$de=="T"),]
pic_4<-data.frame(name=deFA$name,fc=log(deFA$fc),pvalue=-log10(deFA$pvalue),
                  color=c(rep("SCFA",12),rep("MCFA",30-12),rep("LCFA",72-30)))
# pic_4<-pic_4[-27,]
pic_4de<-pic_4[which(pic_4$pvalue>3),]
# pic_4de$name[10]="***-octadecatrienoic acid"
ggplot()+geom_point(data=pic_4[72:1,],mapping = aes(x=fc,y=pvalue,color=color,size=abs(fc)))+
  theme_classic()+scale_color_manual(values=c("#055070","#68AFCE","#CB6481"))