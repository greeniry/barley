library(readxl)
library(stringr)
library(openxlsx)
library(KEGGREST)
library(ggplot2)
data=read_xlsx(paste(getwd(),"/support/结肠内容物.xlsx",sep=""),sheet = 2)
info=data[,c(1,2)]
info=cbind(info,ID="",Name="")
for (i in 1:797)
{
  t<-keggFind("compound",info$Description[i])
  if (length(t)>0)
  {
    info$ID[i]=names(t)[1]
    info$Name[i]=unname(t)[1]
  }
}
load(paste(getwd(),"/support/keggFind.RData",sep=""))
info$ID=substr(info$ID,5,10)
data=cbind(info,data[,-c(1,2)])
write.xlsx(data,paste(getwd(),"/support/kegg_结肠内容物.xlsx",sep=""))

data<-read_xlsx(paste(getwd(),"/support/kegg_结肠内容物.xlsx",sep=""))
unnamedata<-data[which(is.na(data$ID)),]
nameddata<-data[which(!is.na(data$ID)),]
ids=unique(nameddata$ID)
cleardata=nameddata[1,]
cleardata=as.data.frame(cleardata)
for (i in 1:length(ids))
{
  t<-which(nameddata$ID==ids[i])
  if (length(t)==1)
    cleardata=rbind(cleardata,nameddata[t[1],])
  if (length(t)>1)
  {
    cleardata=rbind(cleardata,nameddata[t[1],])
    cleardata[nrow(cleardata),5:46]=apply(as.matrix(nameddata[t,5:46]),2,sum)
  }
}
cleardata=cleardata[-1,]
cleardata=cleardata[order(cleardata$ID),]
rm(nameddata)
data=rbind(cleardata,unnamedata)
info=cleardata[,1:4]
info=cbind(info,Pathway="")

for (i in 1:341)
{
  t<-keggGet(info$ID[i])[[1]]$PATHWAY
  info$Pathway[i]=paste(paste(unname(t),"(",names(t),")",sep=""),collapse="; ")
  Sys.sleep(2)
}

load(paste(getwd(),"/support/pathwayjiechang.RData",sep=""))
hsa=paste(str_split(unname(hsa)," - ",simplify = T)[,1],"(",names(hsa),")",sep="")
hsaid=substr(hsa,str_length(hsa)-5,str_length(hsa)-1)
kegg_onehot=data.frame(
  KEGGID="",
  KEGGPathway=""
)
for (i in 1:341)
{
  if (info$Pathway[i]!=""&info$Pathway[i]!="()")
  {
    t<-str_split(info$Pathway[i],"; ")[[1]]
    for (j in 1:length(t))
    {
      if (length(which(hsaid==substr(t[j],str_length(t[j])-5,str_length(t[j])-1))))
      {
        kegg_onehot<-rbind(
          kegg_onehot,data.frame(
            KEGGID=info$ID[i],
            KEGGPathway=paste(substr(t[j],1,str_length(t[j])-9),"hsa",
                              substr(t[j],str_length(t[j])-5,str_length(t[j])),sep="")))
      }
    }
  }
}
kegg_onehot=kegg_onehot[-1,]
data=data[,-c(1:4)]
data=as.matrix(data)
colnames(data)=paste(substr(colnames(data),1,str_length(colnames(data))-31),
                     substr(colnames(data),str_length(colnames(data))-6,str_length(colnames(data))),
                     sep="")
#3xtg
{
  data=data[,c(1:12,25:30)]
  data=log(data)
  DE=as.matrix(cbind(FC=rep(0,779),Pvalue=rep(0,779)))
  for (i in 1:779) {
    DE[i,1]=sum(exp(data[i,1:6]))/sum(exp(data[i,7:12]))
    DE[i,2]=t.test(data[i,1:6],data[i,7:12])[["p.value"]]
  }
  sel=(DE[,2]<0.05)&((DE[,1]<0.67)|DE[,2]>1.5)
  de_info=info[sel[1:341],]
  kegg_onehot_3xtg=data.frame(
    KEGGID="",
    KEGGPathway=""
  )
  for (i in 1:nrow(de_info))
  {
    if (de_info$Pathway[i]!=""&de_info$Pathway[i]!="()")
    {
      t<-str_split(de_info$Pathway[i],"; ")[[1]]
      for (j in 1:length(t))
      {
        if (length(which(hsaid==substr(t[j],str_length(t[j])-5,str_length(t[j])-1))))
        {
          kegg_onehot_3xtg<-rbind(
            kegg_onehot_3xtg,data.frame(
              KEGGID=de_info$ID[i],
              KEGGPathway=paste(substr(t[j],1,str_length(t[j])-9),"hsa",
                                substr(t[j],str_length(t[j])-5,str_length(t[j])),sep="")))
        }
      }
    }
  }
  kegg_onehot_3xtg=kegg_onehot_3xtg[-1,]
  
  demcount<-length(which(de_info$Pathway!=""&de_info$Pathway!="()"))
  enrichment<-data.frame(
    Pathway=unique(kegg_onehot_3xtg$KEGGPathway),
    superclass="",
    class="",
    Total=0,
    Hits=0,
    Hit_Metabolite="",
    Ratio=0,
    P.value=0
  )
  for (iter in 1:nrow(enrichment))
  {
    id<-substr(
      enrichment$Pathway[iter],
      str_length(enrichment$Pathway[iter])-8,
      str_length(enrichment$Pathway[iter])-1
    )
    rest<-keggGet(id)
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
    Sys.sleep(2)
    print(id)
  }
  
  load(paste(getwd(),"/support/enrichment3xtg.RData",sep=""))
  for (iter in 1:nrow(enrichment))
  {
    enrichment$Total[iter]=length(which(kegg_onehot$KEGGPathway==enrichment$Pathway[iter]))
    temp=kegg_onehot_3xtg$KEGGID[which(kegg_onehot_3xtg$KEGGPathway==enrichment$Pathway[iter])]
    t<-c(0)
    for (it in 1:length(temp))
    {
      t=c(t,which(de_info$ID==temp[it]))
    }
    All=t[-1]
    enrichment$Hits[iter]=length(All)
    enrichment$Hit_Metabolite[iter]=paste(de_info$Description[All],collapse=", ")
    enrichment$Ratio[iter]=enrichment$Hits[iter]/enrichment$Total[iter]
    enrichment$P.value[iter]=fisher.test(matrix(
      c(enrichment$Hits[iter],demcount-enrichment$Hits[iter],
        enrichment$Total[iter]-enrichment$Hits[iter],
        779-demcount-enrichment$Total[iter]+enrichment$Hits[iter]),
      ncol=2))[["p.value"]]
  }
  enrichment_3xtg=enrichment
}
wb <- createWorkbook("Fred")
addWorksheet(wb, "3xTg", tabColour = "#984ea3")
addWorksheet(wb, "5xFAD", tabColour = "#ff7f00")
writeData(wb, sheet = 1, enrichment_3xtg)
#5xfad
{
  data=data[,c(13:24,31:42)]
  data=log(data)
  DE=as.matrix(cbind(FC=rep(0,779),Pvalue=rep(0,779)))
  for (i in 1:779) {
    DE[i,1]=sum(exp(data[i,1:6]))/sum(exp(data[i,7:12]))
    DE[i,2]=t.test(data[i,1:6],data[i,7:12])[["p.value"]]
  }
  sel=(DE[,2]<0.05)&((DE[,1]<0.67)|DE[,2]>1.5)
  de_info=info[sel[1:341],]
  kegg_onehot_5xfad=data.frame(
    KEGGID="",
    KEGGPathway=""
  )
  for (i in 1:nrow(de_info))
  {
    if (de_info$Pathway[i]!=""&de_info$Pathway[i]!="()")
    {
      t<-str_split(de_info$Pathway[i],"; ")[[1]]
      for (j in 1:length(t))
      {
        if (length(which(hsaid==substr(t[j],str_length(t[j])-5,str_length(t[j])-1))))
        {
          kegg_onehot_5xfad<-rbind(
            kegg_onehot_5xfad,data.frame(
              KEGGID=de_info$ID[i],
              KEGGPathway=paste(substr(t[j],1,str_length(t[j])-9),"hsa",
                                substr(t[j],str_length(t[j])-5,str_length(t[j])),sep="")))
        }
      }
    }
  }
  kegg_onehot_5xfad=kegg_onehot_5xfad[-1,]
  
  demcount<-length(which(de_info$Pathway!=""&de_info$Pathway!="()"))
  enrichment<-data.frame(
    Pathway=unique(kegg_onehot_5xfad$KEGGPathway),
    superclass="",
    class="",
    Total=0,
    Hits=0,
    Hit_Metabolite="",
    Ratio=0,
    P.value=0
  )
  for (iter in 1:nrow(enrichment))
  {
    id<-substr(
      enrichment$Pathway[iter],
      str_length(enrichment$Pathway[iter])-8,
      str_length(enrichment$Pathway[iter])-1
    )
    rest<-keggGet(id)
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
    print(id)
  }
  
  load(paste(getwd(),"/support/enrichment5xfad.RData",sep=""))
  for (iter in 1:nrow(enrichment))
  {
    enrichment$Total[iter]=length(which(kegg_onehot$KEGGPathway==enrichment$Pathway[iter]))
    temp=kegg_onehot_5xfad$KEGGID[which(kegg_onehot_5xfad$KEGGPathway==enrichment$Pathway[iter])]
    t<-c(0)
    for (it in 1:length(temp))
    {
      t=c(t,which(de_info$ID==temp[it]))
    }
    All=t[-1]
    enrichment$Hits[iter]=length(All)
    enrichment$Hit_Metabolite[iter]=paste(de_info$Description[All],collapse=", ")
    enrichment$Ratio[iter]=enrichment$Hits[iter]/enrichment$Total[iter]
    enrichment$P.value[iter]=fisher.test(matrix(
      c(enrichment$Hits[iter],demcount-enrichment$Hits[iter],
        enrichment$Total[iter]-enrichment$Hits[iter],
        779-demcount-enrichment$Total[iter]+enrichment$Hits[iter]),
      ncol=2))[["p.value"]]
  }
  enrichment_5xfad=enrichment
}
writeData(wb, sheet = 2, enrichment_5xfad)
saveWorkbook(wb, paste(getwd(),"/support/KEGG-DEM.xlsx",sep=""), overwrite = TRUE)
library(ggrepel)
for (group in 1:2)
{ 
  enrichment<-read_xlsx(paste(getwd(),"/support/KEGG-DEM.xlsx",sep=""),sheet=group)
  id=which(enrichment$P.value<0.05)
  alldata=data.frame(
    ID=substr(enrichment$Pathway[id],1,str_length(enrichment$Pathway[id])-10),
    Ratio=enrichment$Ratio[id],
    `-Log10(P.value)`=-log10(enrichment$P.value[id]),
    Hits=enrichment$Hits[id]
  )
  id2=which(enrichment$P.value<0.05)
  biaozhudata=data.frame(
    ID=substr(enrichment$Pathway[id2],1,str_length(enrichment$Pathway[id])-10),
    Ratio=enrichment$Ratio[id2],
    `-Log10(P.value)`=-log10(enrichment$P.value[id2]),
    Hits=enrichment$Hits[id2]
  )
  ggplot()+geom_point(data=alldata,aes(x=Ratio,y=X.Log10.P.value.,
                                       color=X.Log10.P.value.,
                                       size=Hits))+
    scale_size_continuous(name="Hits",breaks=c(1,2,4,6,8),range=c(2,6))+
    scale_color_continuous(name="-log10(P.value)",high = "#CE1256",low = "#41b6c4")+
    theme_classic()+
    geom_text_repel(data=biaozhudata,aes(x=Ratio,
                                         y=X.Log10.P.value.,
                                         label=ID),max.overlaps=21)+
    labs(x="Ratio",y="-log10(P.value)")
  alldata$ID=factor(alldata$ID,levels = alldata$ID[order(alldata$X.Log10.P.value.)])
  ggplot(data=alldata,aes(x=Hits,y=ID,fill=X.Log10.P.value.))+
    geom_bar(stat = "identity",width = 0.5)+theme_classic()+
    scale_fill_continuous(name="-log10(P.value)",high = "#CE1256",low = "#41b6c4")
}
