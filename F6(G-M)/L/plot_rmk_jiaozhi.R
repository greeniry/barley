load(paste(substr(getwd(),1,stringr::str_length(getwd())-2),"/support/d_rmk_del_jiaozhi.RData",sep=""))
library(ggplot2)
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
tsne_result<-cbind(tsne_result,type=c(rep("Homeostatic",66459),rep("Activated",292537)))

colorset=c("#4d869e","#cc82a5","#CE1256")
ids=c(Homeostatic_id,activated_id)
pdata=data.frame(type=factor(rep(c("WT CD","AD CD","AD BL"),2),levels = c("WT CD","AD CD","AD BL")),
                 cell=rep(c("Homeostatic","Activated"),each=3),
                 counts=c(
                   length(intersect(which(jiaozhi_info_nao$group4=="T_A"),Homeostatic_id))/length(which(jiaozhi_info_nao$group4[ids]=="T_A"))*100,
                   length(intersect(which(jiaozhi_info_nao$group4=="T_B"),Homeostatic_id))/length(which(jiaozhi_info_nao$group4[ids]=="T_B"))*100,
                   length(intersect(which(jiaozhi_info_nao$group4=="T_C"),Homeostatic_id))/length(which(jiaozhi_info_nao$group4[ids]=="T_C"))*100,
                   length(intersect(which(jiaozhi_info_nao$group4=="T_A"),activated_id))/length(which(jiaozhi_info_nao$group4[ids]=="T_A"))*100,
                   length(intersect(which(jiaozhi_info_nao$group4=="T_B"),activated_id))/length(which(jiaozhi_info_nao$group4[ids]=="T_B"))*100,
                   length(intersect(which(jiaozhi_info_nao$group4=="T_C"),activated_id))/length(which(jiaozhi_info_nao$group4[ids]=="T_C"))*100
                 ))
ggplot(pdata, aes(x=type,y=counts,fill=cell)) +
  geom_col(position = 'fill')+theme_classic()+
  scale_fill_manual(values=colorset)
print(length(intersect(which(jiaozhi_info_nao$group4=="T_A"),DAM_id))/length(intersect(which(jiaozhi_info_nao$group4=="T_A"),activated_id)))
print(length(intersect(which(jiaozhi_info_nao$group4=="T_B"),DAM_id))/length(intersect(which(jiaozhi_info_nao$group4=="T_B"),activated_id)))
print(length(intersect(which(jiaozhi_info_nao$group4=="T_C"),DAM_id))/length(intersect(which(jiaozhi_info_nao$group4=="T_C"),activated_id)))

