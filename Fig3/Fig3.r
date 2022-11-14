####Fig3BC####
library(ggplot2)
library(dplyr)
library(ggsignif)
mydata <- read.table("Frequency.txt",header = T,sep="\t",na.strings = "NA")
databar$treatment <- fct_inorder(databar$treatment)
g <- ggplot(mydata, aes(time, values, fill = time)) + facet_wrap(~treatment,scales="free",ncol=4) +
  geom_point(shape = 21, size =2, show.legend = F,position = position_jitterdodge(jitter.width = 0.2)) +
  scale_fill_manual(values = c("grey","#1c689e")) +
  scale_color_manual(values = c("grey","#1c689e")) +# 修改颜色
  geom_line(data = filter(mydata, treatment == "Placebo"), aes(group = ID),colour='gray') + # 添加疫苗组的点对点线条
  geom_line(data = filter(mydata, treatment == "6M4"), aes(group = ID),colour='gray') + # 添加安慰剂组的点对点线条
  geom_line(data = filter(mydata, treatment == "8M1"), aes(group = ID),colour='gray') + # 添加疫苗组的点对点线条
  geom_line(data = filter(mydata, treatment == "L6"), aes(group = ID),colour='gray') + # 添加安慰剂组的点对点线条
  xlab(NULL) +        # 修改x轴的标签
  ylab(NULL) + # 修改y轴的标签
  theme_bw() +
  geom_signif(comparisons = list(c("T0","T1")),
              step_increase =0,
              map_signif_level = T,
              test = wilcox.test,
              tip_length = 0)
g + x11(width =7,height = 3)


####图3 abfA gc####Fig3D
library(dplyr) 
library(ggplot2)
library(ggpubr)
databar=read.csv(file='cluster_ppm.csv',header = T,stringsAsFactors = F)
e <- ggplot(databar, aes(x = Group1, y = Score,fill=Time)) +
  geom_boxplot(aes(color = Time), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black") +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 2, show.legend = F,position = position_jitterdodge(jitter.width = 0.2)) +
  theme(panel.grid = element_blank(), panel.background = element_blank(),outlier.shape=NA) + 
  theme_classic() + theme(panel.grid=element_blank()) + xlab(NULL) + ylab(NULL) +
  scale_fill_manual(values = c("grey","#1c689e")) +
  scale_color_manual(values = c("grey","#1c689e")) + 
  theme(text=element_text(size=16,  family="serif")) +  
  stat_compare_means(aes(group=Time), method = "wilcox.test",label="p.signif",label.y=c(4.1,4.1,4.1))+
  geom_signif(annotations = c("","",""),
              y_position = c(4.05,4.05,4.05),
              xmin = c(0.8,1.8,2.8),
              xmax = c(1.2,2.2,3.2),
              tip_length = 0)
e + x11(width = 4,height = 3)


####图3 基因簇改变量与便秘症状改变量相关性####Fig3E
library(ggpmisc)
library(ggplot2)
library(ggpubr)
library(forcats)
all <- read.table("DEIERTA.txt",header=T,row.names=1,sep="\t")
p <- ggplot(all,aes(x = Delta_Cluster_abundance,y= Delta_Frequency)) +
  geom_point(size=2,aes(x = Delta_Cluster_abundance,y= Delta_Frequency,fill = group,colour =group))+ geom_smooth(method = "lm",colour="#0b0b09") +  
  theme_bw() + theme(panel.grid=element_blank()) + stat_cor(method = "spearman") + scale_color_manual(values = c("#0e946a","#c45b2d","#1c689e"))#+
p + x11(width=3.5,height=2.5)
p1 <- ggplot(all,aes(x = Delta_Cluster_abundance,y= Delta_CCCS)) +
  geom_point(size=2,aes(x = Delta_Cluster_abundance,y= Delta_CCCS,fill = group,colour =group))+ geom_smooth(method = "lm",colour="#0b0b09") +  
  theme_bw() + theme(panel.grid=element_blank()) + stat_cor(method = "spearman") + scale_color_manual(values = c("#0e946a","#c45b2d","#1c689e"))#+
p1 + x11(width=3.5,height=2.5)

p2 <- ggplot(all,aes(x = Delta_Cluster_abundance,y= BSFS)) +
  geom_point(size=2,aes(x = Delta_Cluster_abundance,y= BSFS,fill = group,colour =group))+ geom_smooth(method = "lm",colour="#0b0b09") +  
  theme_bw() + theme(panel.grid=element_blank()) + stat_cor(method = "spearman") + scale_color_manual(values = c("#0e946a","#c45b2d","#1c689e"))
p2
p3 <- ggplot(all,aes(x = Delta_Cluster_abundance,y= HMDAS)) +
  geom_point(size=2,aes(x = Delta_Cluster_abundance,y= BSFS,fill = group,colour =group))+ geom_smooth(method = "lm",colour="#0b0b09") +  
  theme_bw() + theme(panel.grid=element_blank()) + stat_cor(method = "spearman") + scale_color_manual(values = c("#0e946a","#c45b2d","#1c689e"))
p3
####随机森林####Fig3F
library(ggplot2)
library(ggpubr)
library(forcats)
library(ggsci)
df <- read.table(file='dataset_boxplot_others.txt',header=T,row.names=1,sep="\t")
compaired <- list(c("C","H"))
p14 <- ggplot(df, aes(x =group, y = value2,fill=group)) +
  geom_boxplot(aes(color = group), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape=NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 1.5, show.legend = F,position = position_jitterdodge(jitter.width = 0.3)) +  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = 'none',panel.background = element_blank(),axis.text.x = element_text(angle = 0,size =6)) +
  facet_wrap(~index,scales="free",nrow = 1) + ylim(0.1, 2.1) + xlab(NULL) + ylab(NULL) +
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) +
  scale_fill_manual(values = c("grey","#1c689e"))+
  scale_color_manual(values = c("grey","#1c689e"))
p14 + x11(width=2.4, height=2.8)

df <- read.table(file='dataset_boxplot_tis.txt',header=T,row.names=1,sep="\t")
compaired <- list(c("C","H"),c("C","HH"))
p13 <- ggplot(df, aes(x =group, y = value2,fill=group)) +
  geom_boxplot(aes(color = group), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape=NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 1.5, show.legend = F,position = position_jitterdodge(jitter.width = 0.3)) +  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = 'none',panel.background = element_blank(),axis.text.x = element_text(angle = 0,size =6)) +
  facet_wrap(~index,scales="free",nrow = 1) + ylim(0.1, 2.2) + xlab(NULL) + ylab(NULL) +
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) +
  scale_fill_manual(values = c("grey","#1c689e","#1c689e"))+
  scale_color_manual(values = c("grey","#1c689e","#1c689e"))
p13 + x11(width=1.6, height=2.8)
####图3随机森林####Fig3G
library(randomForest)
library(caret)
library(pROC)
set.seed(123)
#发现
dat <- read.table("./dataset_dis.txt",header=TRUE,sep="\t",row.names=1)
trainlist <- createDataPartition(dat$group,p = 0.7,list = FALSE)
trainset <- dat[trainlist,]
testset<- dat[-trainlist,]
rf.train <- randomForest(as.factor(group) ~ .,
                         data = trainset,importance = TRUE,na.action = na.pass)
testprob <- predict(rf.train,newdata = testset,type = "prob")
rocobj <- plot.roc(testset$group, testprob[,1], main="", percent=TRUE,ci=TRUE)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5))
plot(ciobj, type="shape", col="#1c61b6AA",  print.auc=TRUE)
text(40,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(40,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)

#验证
dat_val <- read.table("./dataset_val.txt",header=TRUE,sep="\t",row.names=1)
testprob <- predict(rf.train,newdata = dat_val,type = "prob")
rocobj <- plot.roc(dat_val$group, testprob[,1], main="", percent=TRUE,ci=TRUE)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5))
plot(ciobj, type="shape", col="#1c61b6AA",  print.auc=TRUE)
text(40,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(40,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)