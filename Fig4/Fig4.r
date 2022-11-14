####Fig4BCD####
library(ggplot2)
library(ggpubr)
library(forcats)
all <- read.table("bray_genecluster_bacteria.txt",header=T,row.names=1,sep="\t") 
all$index <- fct_inorder(all$index) 
p<- ggplot(all,aes(x = bray_dist_genus,y= gene_cluster)) +
  geom_smooth(method = "lm",colour="#0b0b09") + 
  facet_wrap(~index,scales="free") +
  geom_point(size=2,aes(x = bray_dist_genus,y= gene_cluster),col = "#2e6b98")+ 
  theme_bw() + theme(panel.grid=element_blank()) + stat_cor(method = "spearman") #+ scale_color_manual(values = c("#0e946a","#c45b2d","#1c689e"))#+
p + x11(width=9.5,height=3)
dev.off() 
dev.new() 
####Fig4E####
library(pheatmap)
library(RColorBrewer)
library(vegan)
library(ComplexHeatmap)
data <- read.table("L6_gene_strain_zenglaing_r.txt",header=T,row.names=1,sep="\t")
pheatmap(data,cluster_col = FALSE,border_size = 0.25,cluster_rows = FALSE,
         show_colnames = T,color = colorRampPalette(c("navy", "white", "firebrick3"))(50))

####Fig4G####
library(ggplot2)
library(ggsci)
library(forcats)
aa=read.csv(file='Ko.txt',header = T,stringsAsFactors = F)
aa = read.delim('Ko.txt', sep = '\t', stringsAsFactors = FALSE)
aa$group <- fct_inorder(aa$group)
p = ggplot(data=aa, mapping=aes(x = type, y = log2FoldChange,fill=Description)) + 
  geom_bar(stat="identity",position=position_dodge()) +
  theme_bw() +  theme(panel.grid=element_blank())+coord_flip() +
  scale_fill_manual(values = c( "#6c6c6c","#274fa3","#da2920")) 