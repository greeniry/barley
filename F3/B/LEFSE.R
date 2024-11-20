library(ggplot2)
library(stringr)
library(microeco)
load(paste(getwd(),"/readin5xfad.RData",sep=""))
ldata=list(data1,data2)
batchname=c("5xFAD_1","5xFAD_2")
rm(data1,data2)
batch = 2
data=ldata[[batch]]
lefsedata=cbind(data[1:2,1:6],data[1:2,8:45])
names<-c("Muribaculaceae","Prevotella","Lactobacillus","Desulfovibrio","Allobaculum",  
         "Lachnospiraceae","Bifidobacterium","Clostridiales","Ruminococcus","Bacteroides",   
         "Sutterella","F16","Coprococcus","Oscillospira","Ruminococcaceae",
         "Rikenellaceae","Butyricicoccus","Clostridium","YS2")
dim<-c(6,6,6,6,6,5,6,4,6,6,6,5,6,6,5,5,6,6,4)
for (i in 1:19)
{
  t<-data[which(data[,dim[i]]==names[i]),]
  lefsedata<-rbind(lefsedata,c(t[1,1:6],apply(t[,8:45],2,sum)))
}
lefsedata<-lefsedata[-c(1:2),]
tax<-lefsedata[,1:6]
tax[1,6]=""
tax[6,6]=""
tax[8,5:6]=""
tax[12,6]=""
tax[15:16,6]=""
tax[19,5:6]=""
tax[,1]<-paste("k__",tax[,1],sep="")
tax[,2]<-paste("p__",tax[,2],sep="")
tax[,3]<-paste("c__",tax[,3],sep="")
tax[,4]<-paste("o__",tax[,4],sep="")
tax[,5]<-paste("f__",tax[,5],sep="")
tax[,6]<-paste("g__",tax[,6],sep="")
# otu=lefsedata[,which(substr(colnames(lefsedata),16,16)=="6")]
# sample=data.frame(id=colnames(otu),group=factor(substr(colnames(otu),10,14),
#                                                 levels = c("WT_CD", "WT_BL", "AD_CD","AD_BL")))
# rownames(sample)=colnames(otu)
# dataset <- microtable$new(sample_table = sample,
#                           otu_table = otu, 
#                           tax_table = tax)
# lefse <- trans_diff$new(dataset = dataset, 
#                         method = "lefse", 
#                         group = "group", 
#                         alpha = 0.05, p_adjust_method = "none",
#                         lefse_subgroup = NULL)
# lefse$plot_diff_bar(width = 0.8, 
#                     group_order = c("WT_CD", "WT_BL", "AD_CD","AD_BL")) +
#   ggsci::scale_color_npg() +
#   ggsci::scale_fill_npg()
# lefse$plot_diff_cladogram(use_taxa_num = 200, 
#                           use_feature_num = 50, 
#                           clade_label_level = 6, 
#                           group_order = c("WT_CD", "WT_BL", "AD_CD","AD_BL"))

otu=lefsedata[,which(substr(colnames(lefsedata),16,16)=="6"&substr(colnames(lefsedata),10,11)=="AD")]
sample=data.frame(id=colnames(otu),group=factor(substr(colnames(otu),10,14),
                                                levels = c("AD_CD","AD_BL")))
rownames(sample)=colnames(otu)
dataset <- microtable$new(sample_table = sample,
                          otu_table = otu, 
                          tax_table = tax)
lefse <- trans_diff$new(dataset = dataset, 
                        method = "lefse", 
                        group = "group", 
                        alpha = 0.05, p_adjust_method = "none",
                        lefse_subgroup = NULL)
lefse$plot_diff_bar(width = 0.8, 
                    group_order = c("AD_CD","AD_BL")) +
  ggsci::scale_color_npg() +
  ggsci::scale_fill_npg()
lefse$plot_diff_cladogram(use_taxa_num = 200, 
                          use_feature_num = 50, 
                          clade_label_level = 1, 
                          group_order = c("AD_CD","AD_BL"),color = c("#055070","#ae1266"))

library(ggplot2)
data=data.frame(names=lefse[["res_diff"]][["Taxa"]],lda=lefse[["res_diff"]][["LDA"]],color=lefse[["res_diff"]][["Group"]])
data$lda[which(lefse[["res_diff"]][["Group"]]=="AD_CD")]=-data$lda[which(lefse[["res_diff"]][["Group"]]=="AD_CD")]
ggplot()+geom_col(data=data,mapping = aes(x=lda,y=names,fill=color))
