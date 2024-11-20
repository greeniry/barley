library(readxl)
data<-read_xlsx(paste(getwd(),"/support/结肠内容物.xlsx",sep=""),sheet = 2)
data<-data[,-c(3:4)]
info<-read.csv(paste(getwd(),"/support/name_map.csv",sep=""))
data<-data[-54,]
data<-cbind(data[,1:2],info[,2:5],data[,3:44])
info<-data[,1:6]
data<-as.matrix(data[,-c(1:6)])
hmdb<-read.csv(paste(getwd(),"/support/hmdb_metabolites.csv",sep=""))
id=0
for (i in 1:nrow(info))
{
  t<-which(hmdb$hmdbid==info$HMDB[i])
  if (length(t)>0)
    id=c(id,which(hmdb$hmdbid==info$HMDB[i]))
  else
    id=c(id,0)
}
id=id[-1]
info=cbind(info,kingdom="",superclass="",class="",subclass="",directparent="",pathway="")
for (i in 1:nrow(info))
{
  if (id[i]!=0)
    info[i,7:12]=hmdb[id[i],4:9]
}
info=cbind(info,fc=0,pvalue=0,de="F")
for (i in 1:nrow(data))
{
  adcd=as.numeric(data[i,19:24])
  adbl=as.numeric(data[i,13:18])
  info[i,14]=summary(aov(data~type,data=data.frame(data=c(log(adcd),log(adbl)),type=c(rep("ADCD",6),rep("WTCD",6)))))[[1]][["Pr(>F)"]][1]
  info[i,13]=mean(adbl)/mean(adcd)
  if (info[i,14]<0.05&(info[i,13]>2|info[i,13]<1/2))
    info[i,15]="T"
}
rm(hmdb,hmdbview,adcd,adbl,t,id,i)
save.image(paste(getwd(),"/support/jiechang.RData",sep=""))
writexl::write_xlsx(info[,-12],paste(getwd(),"/support/jiechang_info.xlsx",sep=""))
