{load(paste(substr(getwd(),1,stringr::str_length(getwd())-3),"/support/d_rmk_del_jiaozhi.RData",sep=""))
  cordata<-data.frame(ID=jiaozhi_info_nao$sample[which(!duplicated(jiaozhi_info_nao$sample))],
                      Type=jiaozhi_info_nao$group4[which(!duplicated(jiaozhi_info_nao$sample))],
                      jiaozhi=0,jushi=0)
  for (i in 1:10)
  {
    cordata[i,3]=length(intersect(which(jiaozhi_info_nao$sample==cordata[i,1]),DAM_id))
    # cordata[i,3]=length(which(jiaozhi_info_nao$sample==cordata[i,1]))
  }
  rm(jiaozhi_data_nao,jiaozhi_info_nao,nao_umap,activated_id,DAM_id,del_id,Homeostatic_id)
  load(paste(substr(getwd(),1,stringr::str_length(getwd())-3),"/support/MDM_5.RData",sep=""))
  for (i in 1:10)
  {
    cordata[i,4]=length(which(substr(info$sample,3,20)==substr(cordata[i,1],3,20)
                              # &(cl_i$cluster==1|cl_i$cluster==2|cl_i$cluster==4)
    ))
  }
  library(ggplot2)
  colorset=c("#7AC39b","#6bb4d4","#d25b9c")
  ggplot(cordata, aes(x=jushi,y=jiaozhi)) +
    geom_point(size=4,aes(color=Type))+theme_classic()+
    scale_color_manual(values=colorset)+geom_smooth(method = 'lm', formula = y ~ x)
  cor(cordata$jiaozhi,cordata$jushi)
  lm(jiaozhi~jushi,data=cordata)
  cor.test(cordata$jiaozhi,cordata$jushi)
}

{load(paste(substr(getwd(),1,stringr::str_length(getwd())-3),"/support/d_rmk_del_jiaozhi.RData",sep=""))
  cordata<-data.frame(ID=jiaozhi_info_nao$sample[which(!duplicated(jiaozhi_info_nao$sample))],
                      Type=jiaozhi_info_nao$group4[which(!duplicated(jiaozhi_info_nao$sample))],
                      jiaozhi=0,jushi=0)
  for (i in 1:10)
  {
    cordata[i,3]=length(intersect(which(jiaozhi_info_nao$sample==cordata[i,1]),DAM_id))
    # cordata[i,3]=length(which(jiaozhi_info_nao$sample==cordata[i,1]))
  }
  rm(jiaozhi_data_nao,jiaozhi_info_nao,nao_umap,activated_id,DAM_id,del_id,Homeostatic_id)
  load(paste(substr(getwd(),1,stringr::str_length(getwd())-3),"/support/MDM_5.RData",sep=""))
  for (i in 1:10)
  {
    cordata[i,4]=length(which(substr(info$sample,3,20)==substr(cordata[i,1],3,20)
                               &(cl_i$cluster==1)
    ))
  }
  library(ggplot2)
  colorset=c("#7AC39b","#6bb4d4","#d25b9c")
  ggplot(cordata, aes(x=jushi,y=jiaozhi)) +
    geom_point(size=4,aes(color=Type))+theme_classic()+
    scale_color_manual(values=colorset)+geom_smooth(method = 'lm', formula = y ~ x)
  cor(cordata$jiaozhi,cordata$jushi)
  lm(jiaozhi~jushi,data=cordata)
  cor.test(cordata$jiaozhi,cordata$jushi)
}


