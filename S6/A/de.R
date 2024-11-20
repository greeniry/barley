library(readxl)
load(paste(getwd(),"/support/jiechang.RData",sep=""))
info[,15]="F"
for (i in 1:nrow(data))
{
  adcd=as.numeric(data[i,7:12])
  adbl=as.numeric(data[i,1:6])
  info[i,14]=summary(aov(data~type,data=data.frame(data=c(log(adcd),log(adbl)),type=c(rep("ADCD",6),rep("WTCD",6)))))[[1]][["Pr(>F)"]][1]
  info[i,13]=mean(adbl)/mean(adcd)
  if (info[i,14]<0.05&(info[i,13]>2|info[i,13]<1/2))
    info[i,15]="T"
}
save.image(paste(getwd(),"/support/info+data.RData",sep=""))