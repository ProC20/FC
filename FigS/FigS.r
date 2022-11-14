####图2 8株菌mars otu####FigS3B
databar <-read.table("mars_od_8bl.txt",header=TRUE,sep="\t",row.names=1)
compaired <- list(c("A","B"),c("A","C"))
databar$group <- fct_inorder(databar$group)
e <- ggplot(databar, aes(x = group, y = value,fill=group)) +
  geom_boxplot(aes(color = group), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape = NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 2, show.legend = F,position = position_jitterdodge(jitter.width = 0.2)) +
  theme(panel.grid = element_blank(), panel.background = element_blank()) +
  expand_limits( y = 0)+
  theme_bw() + theme(panel.grid=element_blank()) +   #facet_wrap(~index,scales="free") +
  scale_fill_manual(values = c("#b92429","#b92429","#b92429","#b92429","#b92429","#b92429","#b92429","#b92429")) +
  scale_color_manual(values = c("#6e6f72","#b82e35","#4363a7","#4363a7","#4363a7","#b92429","#b92429","#b92429")) +
  theme(text=element_text(size=16,  family="serif"),legend.position = "nono",axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)) + xlab(NULL) + ylab(NULL) #+
geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0)
e + x11(width =3,height = 2.5)
####图2####Fig2F
library(ggplot2)
library(ggsignif)
library(forcats)
library(ggsci)
df <- read.table("exp2_xin1.txt",header = T,sep="\t",na.strings = "NA")
df$group <- fct_inorder(df$group)
compaired <- list(c("CTR","LOP"),c("LOP","S32"))
e <- ggplot(df, aes(y=value, x=group))+ facet_wrap(~index,scales="free") + 
  geom_bar(aes(fill=group), position=position_dodge(1),stat="summary",width=0.8,colour = "black") +
  theme_classic() +
  theme(legend.direction = "horizontal", legend.position = "none", axis.text.x = element_text(angle = 45)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(expand = c(0,0))+
  geom_jitter(data = df,  aes(fill=group, y=value, x=group),size = 2, shape = 21) +
  stat_summary(fun.data = 'mean_sdl', geom = "errorbar", colour = "black", width = 0.2,position = position_dodge(1)) + 
  scale_fill_manual(values = c("#cfd0d1","#6e6f72",'#b82e35','#b82e35','#b82e35','#b82e35','#4363a7','#4363a7','#4363a7','#4363a7')) +
  scale_color_manual(values = c("#cfd0d1","#6e6f72",'#b82e35','#b82e35','#b82e35','#b82e35','#4363a7','#4363a7','#4363a7','#4363a7')) +
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) 
e + x11(width =6.5,height = 5)
####图2补充材料 便秘指标####FigS3EF
library(ggplot2)
library(ggsignif)
library(forcats)
library(ggsci)
df <- read.table("index_3.txt",header = T,sep="\t",na.strings = "NA")
df$group <- fct_inorder(df$group)
compaired <- list(c("CTR","LOP"),c("LOP","S32"))
e <- ggplot(df, aes(y=value, x=group))+ facet_wrap(~index,scales="free") + 
  geom_bar(aes(fill=group), position=position_dodge(1),stat="summary",width=0.8,colour = "black") +
  theme_classic() +
  theme(legend.direction = "horizontal", legend.position = "none", axis.text.x = element_text(angle = 45)) +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(expand = c(0,0))+
  geom_jitter(data = df,  aes(fill=group, y=value, x=group),size = 2, shape = 21) +
  stat_summary(fun.data = 'mean_sdl', geom = "errorbar", colour = "black", width = 0.2,position = position_dodge(1)) + 
  scale_fill_manual(values = c("#cfd0d1","#6e6f72",'#b82e35','#b82e35',"#4363a7")) +
  scale_color_manual(values = c("#cfd0d1","#6e6f72",'#b82e35','#b82e35',"#4363a7")) +
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) 
e + x11(width =3.5,height = 2.5)
####acetate_mars_bl_3####FigS3D
databar <-read.table("acetate_mars_bl_3.txt",header=TRUE,sep="\t",row.names=1)
compaired <- list(c("A","B"),c("A","C"))
databar$group <- fct_inorder(databar$group)
e <- ggplot(databar, aes(x = group, y = value,fill=group)) +
  geom_boxplot(aes(color = group), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape = NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 2, show.legend = F,position = position_jitterdodge(jitter.width = 0.2)) +
  theme(panel.grid = element_blank(), panel.background = element_blank()) + 
  theme_bw() + theme(panel.grid=element_blank()) +  
  scale_fill_manual(values = c("#b82e35","#b82e35","#4363a7","#4363a7","#4363a7")) +
  scale_color_manual(values = c("#b82e35","#b82e35","#4363a7","#4363a7","#4363a7")) + 
  theme(text=element_text(size=16,  family="serif"),legend.position = "nono",axis.text.x = element_text(angle = 45)) + xlab(NULL) + ylab(NULL) #+  
geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) 
e + x11(width =1.5,height = 2.5)
