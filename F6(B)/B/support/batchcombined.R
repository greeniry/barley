library(openxlsx)
library(readxl)
data_1<-read_xlsx(paste(getwd(),"/support/Intensity_all_add_kegg.xlsx",sep=""))
data_2<-read_xlsx(paste(getwd(),"/support/Intensity_all_2.xlsx.xlsx",sep=""))
info_1<-data_1[,1:10]
data_1<-data_1[,11:56]
# data_1<-data_1[,c(51:52,48:50,30:31,23:29)]
# colnames(data_1)=c("WT_CD_1","WT_CD_2","WT_BL_1","WT_BL_2","WT_BL_3",
#                    "AD_CD_1","AD_CD_2","AD_BL_1","AD_BL_2","AD_BL_3",
#                    "AD_BL_4","AD_BL_5","AD_BL_6","AD_BL_7")
data_1<-as.matrix(data_1)
info_2<-data_2[,1:20]
data_2<-data_2[,19:37+2]
colnames(data_2)=c("QC_1","QC_2","QC_3","QC_4","WT_CD_1","WT_CD_2",
                   "WT_CD_3","WT_BL_1","WT_BL_2","WT_BL_3",
                   "AD_CD_1","AD_CD_2","AD_CD_3","AD_BL_1","AD_BL_2","AD_BL_3",
                   "AD_BL_4","AD_BL_5","AD_BL_6")

ids=intersect(info_2$Name2,info_1$Description)
ids=ids[-3]
data_1_index=0
for (i in 1:length(ids))
{
  data_1_index<-c(data_1_index,which(info_1$Description==ids[i])[1])
}
data_1_index=data_1_index[-1]
data_2_index=0
for (i in 1:length(ids))
{
  data_2_index<-c(data_2_index,which(info_2$Name2==ids[i]))
}
data_2_index=data_2_index[-1]
data_2<-as.matrix(data_2)
colnames(data_1)<-paste("1_",colnames(data_1),sep="")
colnames(data_2)<-paste("2_",colnames(data_2),sep="")
colnames(info_1)<-paste("1_",colnames(info_1),sep="")
colnames(info_2)<-paste("2_",colnames(info_2),sep="")
data<-cbind(data_1[data_1_index,],data_2[data_2_index,])
info<-cbind(info_1[data_1_index,],info_2[data_2_index,])
rm(data_1,info_1,data_2,info_2,data_1_index,data_2_index,i,ids)
# scfa_index=c(4,5,13,21,27,28,30)
# data=t(data)
# data=as.data.frame(data)
# data=log(data)
# data=data[,scfa_index]
dingsuan_index=c(4,5)
data=t(data)
data=as.data.frame(data)
data=log(data)
# data=data[,dingsuan_index]
data=cbind(Samples=rownames(data),Class=c(rep("S",42),rep("QC",8),rep("S",15)),
           Batch=c(rep("B1",46),rep("B2",19)),Order=1:65,data)
write.csv(data,paste(getwd(),"/support/combined.csv",sep=""))

library(MetaboAnalystR)
mSet <- InitDataObjects("list", "utils", FALSE)
mSet <- Read.BatchDataTB(mSet,missingEstimate = 0,
                         paste(getwd(),"/support/combined.csv",sep=""),"row")
mSet <- PerformBatchCorrection(mSet)
data2<-t(mSet[["dataSet"]][["Combat_edata"]])
data2<-data2[,c(c(51:52,48:50,30:31,23:29)-10,51:65)]
colnames(data2)[1:14]=c("1_WT_CD_1","1_WT_CD_2","1_WT_BL_1","1_WT_BL_2","1_WT_BL_3",
                   "1_AD_CD_1","1_AD_CD_2","1_AD_BL_1","1_AD_BL_2","1_AD_BL_3",
                   "1_AD_BL_4","1_AD_BL_5","1_AD_BL_6","1_AD_BL_7")
save.image(paste(getwd(),"/support/batchcombined.RData",sep=""))