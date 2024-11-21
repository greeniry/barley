# This is support code to generate del_new_rmk.RData, d_rmk_del_jiaozhi.RData and ACT_10_umap.RData.

#load brain original data
load("/data/yuzhou/MarkerExpression.RData")
data_nao<-data[which(data$group4!=""),]
info_nao<-data_nao[,1:5]
data_nao<-data_nao[,-c(1:5)]
library(umap)
nao_umap<- umap(data_nao,preserve.seed = TRUE, method = c("naive"))
# save.image("/data/yuzhou/d_f_umap_naive.RData")
# library(ggplot2)
tsne_result = as.data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
cl_i <- kmeans(f_data_nao, centers = 30, nstart = 3,algorithm="Lloyd")
filted_id=which(cl_i$cluster!=1&cl_i$cluster!=17&
                  cl_i$cluster!=18&cl_i$cluster!=2&
                  cl_i$cluster!=20&cl_i$cluster!=26&
                  cl_i$cluster!=28&cl_i$cluster!=29&cl_i$cluster!=7)
f_data_nao=data_nao[filted_id,]
f_info_nao=info_nao[filted_id,]
rm(data,data_nao,info_nao,cl_i,p1,tsne_result,pdata,filted_id,nao_umap)
nao_umap<- umap(f_data_nao,preserve.seed = TRUE, method = c("naive"))
tsne_result = as.data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
cl_i <- kmeans(f_data_nao, centers = 10, nstart = 3,algorithm="Lloyd")
jiaozhi_id<-which(cl_i$cluster=="5"|cl_i$cluster=="6"|cl_i$cluster=="7"|cl_i$cluster=="8"|cl_i$cluster=="10")
jiaozhi_data_nao<-f_data_nao[jiaozhi_id,]
jiaozhi_info_nao<-f_info_nao[jiaozhi_id,]

feijiaozhi_data_nao<-f_data_nao[-jiaozhi_id,]
feijiaozhi_info_nao<-f_info_nao[-jiaozhi_id,]
save.image("/data/yuzhou/mianyi.RData")
library(umap)
load("/data/yuzhou/mianyi.RData")
nao_umap<- umap(jiaozhi_data_nao,preserve.seed = TRUE, method = c("naive"))
save.image("/data/yuzhou/new_mianyi_jiaozhi.RData")
library(umap)
load("/data/yuzhou/mianyi.RData")
nao_umap<- umap(feijiaozhi_data_nao,preserve.seed = TRUE, method = c("naive"))
save.image("/data/yuzhou/new_mianyi_feijiaozhi.RData")


f_info_nao$group3="undefined"
rownames(f_info_nao)=f_info_nao$X
f_info_nao[as.character(rmk_feijiaozhi_info$X),3]=rmk_feijiaozhi_info$group3
f_info_nao[as.character(jiaozhi_info_nao$X),3]="Microglia"
new_data_nao=f_data_nao[which(f_info_nao$group3!="undefined"),]
new_info_nao=f_info_nao[which(f_info_nao$group3!="undefined"),]
rm(f_info_nao,f_data_nao,centers,colorset,i,id)
nao_umap<- umap(new_data_nao,preserve.seed = TRUE, method = c("naive"))

centers=35
tsne_result = as.data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
# id=which(tsne_result$umap2> -10&tsne_result$umap1> -10)
# tsne_result=tsne_result[id,]
# jiaozhi_data_nao=jiaozhi_data_nao[id,]
# jiaozhi_info_nao=jiaozhi_info_nao[id,]
cl_i <- kmeans(new_data_nao, centers = centers, nstart = 3,algorithm="Lloyd")

testid=which(cl_i$cluster==1|cl_i$cluster==3|cl_i$cluster==17|cl_i$cluster==27|
               cl_i$cluster==28|cl_i$cluster==30|cl_i$cluster==35)

tsne_result = as.data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")

# p1<-ggplot(tsne_result[-testid,],aes(umap1,umap2,color=as.character(cl_i$cluster[-testid]))) +
#   geom_point() +scale_color_manual(values = colorset[1:28])
# ggsave(paste("C:/Users/li08/Desktop/barley/Fcytof/B/test2",centers,"-f_umap.png",sep=""),
#        p1,units="in",width = 10,height = 10,dpi=300)
del_id1=intersect(which(tsne_result$umap1<5&tsne_result$umap2>-5),
                 which(cl_i$cluster==1|cl_i$cluster==3|cl_i$cluster==17|cl_i$cluster==27|
                         cl_i$cluster==28|cl_i$cluster==30|cl_i$cluster==35))
del_id2=intersect(which(tsne_result$umap1>6|tsne_result$umap2< -5),
                  which(cl_i$cluster!=1&cl_i$cluster!=3&cl_i$cluster!=17&cl_i$cluster!=27&
                          cl_i$cluster!=28&cl_i$cluster!=30&cl_i$cluster!=35) )
tsne_result=tsne_result[-c(del_id1,del_id2),]
new_data_nao=new_data_nao[-c(del_id1,del_id2),]
new_info_nao=new_info_nao[-c(del_id1,del_id2),]
nao_umap$layout=nao_umap$layout[-c(del_id1,del_id2),]
cl_i$cluster=cl_i$cluster[-c(del_id1,del_id2)]

testid=which(cl_i$cluster==1|cl_i$cluster==3|cl_i$cluster==17|cl_i$cluster==27|
               cl_i$cluster==28|cl_i$cluster==30|cl_i$cluster==35)

feijiaozhi_id=testid

new_info_nao$group3="Microglia"
new_info_nao$group3[which(cl_i$cluster==1)]="DCs"
new_info_nao$group3[which(cl_i$cluster==3)]="Macrophages"
new_info_nao$group3[which(cl_i$cluster==17)]="Migratory DCs"
new_info_nao$group3[which(cl_i$cluster==27)]="Neutrophils"
new_info_nao$group3[which(cl_i$cluster==28)]="B cells"
new_info_nao$group3[which(cl_i$cluster==30)]="CD4+T cells"
new_info_nao$group3[which(cl_i$cluster==35)]="CD8+T cells"

rm(cl_i,p1,pdata,testid,centers,tsne_result,colorset,del_id1,del_id2,feijiaozhi_id,i)

save.image("/data/yuzhou/del_new_rmk.RData")

jiaozhi_info_nao=new_info_nao[which(new_info_nao$group3=="Microglia"),]
jiaozhi_data_nao=new_data_nao[which(new_info_nao$group3=="Microglia"),]
centers=40
tsne_result = data.frame(nao_umap$layout)
colnames(tsne_result) = c("umap1","umap2")
delid2<-which(tsne_result$umap2< -10)
jiaozhi_data_nao=jiaozhi_data_nao[-delid2,]
jiaozhi_info_nao=jiaozhi_info_nao[-delid2,]
tsne_result=tsne_result[-delid2,]
cl_i <- kmeans(jiaozhi_data_nao[,-del_id2], centers = centers, nstart = 3,algorithm="Lloyd")
Homeostatic_id<-which(cl_i$cluster==8|cl_i$cluster==16|cl_i$cluster==18|cl_i$cluster==27|
                        cl_i$cluster==31|cl_i$cluster==33)
activated_id<-which(cl_i$cluster==1|cl_i$cluster==2|cl_i$cluster==4|cl_i$cluster==6|
                      cl_i$cluster==9|cl_i$cluster==10|cl_i$cluster==11|cl_i$cluster==13|
                      cl_i$cluster==14|cl_i$cluster==15|(cl_i$cluster>=20&cl_i$cluster<=26)|
                      cl_i$cluster==28|cl_i$cluster==29|cl_i$cluster==30|cl_i$cluster==32|
                      (cl_i$cluster>=34&cl_i$cluster<=40))
DAM_id<-which(cl_i$cluster==1|cl_i$cluster==4|cl_i$cluster==10|cl_i$cluster==11|
                cl_i$cluster==22|cl_i$cluster==26|cl_i$cluster==28|cl_i$cluster==36|
                cl_i$cluster==40)
rm(cl_i,nao_umap,p1,pdata,tsne_result,centers,colorset,delid2,i,sel_id)
new_data_nao=rbind(new_data_nao[which(new_info_nao$group3!="Microglia"),],jiaozhi_data_nao[c(activated_id,Homeostatic_id),])
new_info_nao=rbind(new_info_nao[which(new_info_nao$group3!="Microglia"),],jiaozhi_info_nao[c(activated_id,Homeostatic_id),])

save.image("/data/yuzhou/del_new_rmk.RData")



jiaozhi_data_nao<-new_data_nao[which(new_info_nao$group3=="Microglia"),]
jiaozhi_info_nao<-new_info_nao[which(new_info_nao$group3=="Microglia"),]
rm(nao_umap,new_data_nao,new_info_nao)
del_id=c(3,17,18,22,28,33,34,35,36,37,40,42)

library(umap)
nao_umap<- umap(jiaozhi_data_nao[c(Homeostatic_id,activated_id),-del_id],preserve.seed = TRUE, method = c("naive"))
save.image("/data/yuzhou/d_rmk_del_jiaozhi.RData")

data=jiaozhi_data_nao[activated_id,]
info=jiaozhi_info_nao[activated_id,]
nao_umap<- umap(data[,-del_id],preserve.seed = TRUE, method = c("naive"))
# data=data[which(nao_umap$layout[,1]<20&nao_umap$layout[,2]>-5),]
# info=info[which(nao_umap$layout[,1]<20&nao_umap$layout[,2]>-5),]
# nao_umap$layout=nao_umap$layout[which(nao_umap$layout[,1]<20&nao_umap$layout[,2]>-5),]
centers=10
{
  tsne_result = data.frame(nao_umap$layout)
  colnames(tsne_result) = c("umap1","umap2")
  cl_i <- kmeans(data[,-del_id], centers = centers, nstart = 3,algorithm="Lloyd")
  save.image("/data/yuzhou/ACT_10_umap.RData")
}



