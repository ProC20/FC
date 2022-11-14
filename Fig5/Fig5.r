####Fig5A####
####图5 热图####
library(pheatmap)
library(RColorBrewer)
library(vegan)
library(ComplexHeatmap)
data <-read.table("gene_ko.txt",header=TRUE,sep="\t",row.names=1)
data <- read.table("T0_daixie_clinical_heatmap.txt",header=T,row.names=1,sep="\t")
data <- read.table("clinical_daixie_heatmap.txt",header=T,row.names=1,sep="\t")
pheatmap(data,cluster_col = FALSE,border_size = 0.25,cluster_rows = FALSE,
         show_colnames = T)
####Fig5B####
library(ggplot2)
library(ggpubr)
library(forcats)
library(ggsci)
compaired <- list(c("6M4_T0","6M4_T1"),c("8M1_T0","8M1_T1"),c("L6_T0","L6_T1"))#,c("L6_T0","6M4_T0"),c("8M1_T0","6M4_T0"),c("8M1_T0","L6_T0"))
df <- read.table(file='daixie7.txt',header=T,row.names=1,sep="\t")
df$index <- fct_inorder(df$index)
p11 <- ggplot(df, aes(x=group1, y=value2)) + geom_boxplot(aes(color = time),outlier.shape=NA)+theme_bw() + labs(title = "", y=" ", x = "") +
  theme(panel.grid=element_blank(),legend.position = 'none' ,  axis.text.x = element_text(angle = 0)) +
  facet_wrap(~index,scales="free",nrow = 2) +  geom_jitter(aes(colour = time),width = 0.2) + 
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) + 
  scale_color_manual(values = c("#6c6c6c","#5dabd3")) + scale_fill_manual(values = c("#CAB2D6","#b4b4b4"))
p11 + x11(width=8.27, height=4.5)
#
p12 <- ggplot(df, aes(x = group1, y = value2,fill=time)) +
  geom_boxplot(aes(color = time), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape=NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 1.5, show.legend = F,position = position_jitterdodge(jitter.width = 0.3)) +  theme_bw() +
  theme(panel.grid = element_blank(), legend.position = 'none',panel.background = element_blank(),axis.text.x = element_text(angle = 0,size =6)) + 
  facet_wrap(~index,scales="free",nrow = 2) + 
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) + 
  scale_fill_manual(values = c("grey","#1c689e")) +
  scale_color_manual(values = c("grey","#1c689e")) 
p12 + x11(width=8.27, height=4)
####Fig5D####
M <- read.delim('MK.txt', sep = '\t', stringsAsFactors = FALSE)
K <- read.delim('KB1.txt', sep = '\t', stringsAsFactors = FALSE)
I <- read.delim('BI1.txt', sep = '\t', stringsAsFactors = FALSE)
circ_mi_m <- merge(M,K, by = 'K')
circ_mi_m <- merge(circ_mi_m,I, by = 'B')
library(ggalluvial)
color_circRNA <- c('#db9a2f','#5dabd3' ,'#0e946a','#91D1C2FF', '#1c689e', '#c45b2d','#bf7199', '#FFFF33')
color_miRNA <- c( '#FFED6F', '#E41A1C', '#377EB8','#A65628', '#FFFFB3', '#BEBADA')
color_mRNA<- c( '#aa0511', '#043a9b', '#037c6d')
t <- c("white","white","white","white","white","white","white")
library(ggalluvial)
p <- ggplot(circ_mi_m, aes(axis1 = M, axis2 = K, axis3= B, axis4 = I)) +
  geom_flow(aes(fill = I, color=M)) +  #在这里，将所有连线的颜色按 miRNA 赋值，代表 miRNA 介导的关系
  geom_stratum() +
  scale_color_manual(values = rev(t))+ scale_fill_manual(values = rev(color_mRNA))+
  geom_text(stat = 'stratum', infer.label = TRUE, size = 3.5) +
  scale_x_discrete(limits = c('M','K', 'B', 'I')) +
  labs(x = '', y = '') +  #从这儿开始是一些主题参数等的设置
  theme(legend.position = 'none', panel.background = element_blank(),
        line = element_blank(), text = element_blank())
p + X11(width = 10, height = 5)