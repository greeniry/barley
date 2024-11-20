library(ggplot2)
library(stringr)
library(microeco)
load(paste(getwd(),"/readin.RData",sep=""))
ldata=list(data1,data2,data3,data4)
batchname=c("3xTg_1","3xTg_2","3xTg_1+2","3xTg_3+1(cd)")
rm(data1,data2,data3,data4)
batch=4
data=ldata[[batch]]
data$Genus[which(data$Family=="S24-7")]="Muribaculaceae"
data$Family[which(data$Family=="S24-7")]="Muribaculaceae"

lefsedata=cbind(data[1:2,1:6],data[1:2,8:74])
names<-c("Muribaculaceae","Prevotella","Clostridiales","Oscillospira",
         "Lachnospiraceae","Allobaculum","Ruminococcaceae","Sutterella",
         "Bacteroidales","Rikenellaceae","YS2","Lactobacillus",
         "Ruminococcus","Odoribacter","Bifidobacterium","Bacteroides")
dim<-c(6,6,4,6,5,6,5,6,4,5,4,6,6,6,6,6)
for (i in 1:16)
{
  t<-data[which(data[,dim[i]]==names[i]),]
  lefsedata<-rbind(lefsedata,c(t[1,1:6],apply(t[,8:74],2,sum)))
}
lefsedata<-lefsedata[-c(1:2),]
tax<-lefsedata[,1:6]
tax[1,6]=""
tax[3,5:6]=""
tax[5,6]=""
tax[7,6]=""
tax[9,5:6]=""
tax[10,6]=""
tax[11,5:6]=""
tax[,1]<-paste("k__",tax[,1],sep="")
tax[,2]<-paste("p__",tax[,2],sep="")
tax[,3]<-paste("c__",tax[,3],sep="")
tax[,4]<-paste("o__",tax[,4],sep="")
tax[,5]<-paste("f__",tax[,5],sep="")
tax[,6]<-paste("g__",tax[,6],sep="")
# otu=lefsedata[,which(substr(colnames(lefsedata),15,15)=="6")]
# sample=data.frame(id=colnames(otu),group=factor(substr(colnames(otu),9,13),
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

otu=lefsedata[,which(substr(colnames(lefsedata),15,15)=="6"&substr(colnames(lefsedata),9,10)=="AD")]
sample=data.frame(id=colnames(otu),group=factor(substr(colnames(otu),9,13),
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
