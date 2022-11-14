##无菌鼠生理指标####Fig6BC
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(forcats)
library(ggsignif)
databar=read.csv(file='FO_all.csv',header = T,stringsAsFactors = F)
compaired <- list(c("H_pre","H_post"),c("L_pre","L_post"))
databar$group <- fct_inorder(databar$group)
e <- ggplot(databar, aes(x = group, y = value,fill=group)) +
  geom_boxplot(aes(color = group), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape = NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 2, show.legend = F,position = position_jitterdodge(jitter.width = 0.2)) +
  theme(panel.grid = element_blank(), panel.background = element_blank()) + 
  theme_bw() + theme(panel.grid=element_blank()) +   facet_wrap(~index,scales="free",ncol=4) +
  scale_fill_manual(values = c("grey","#1c689e","grey","#1c689e")) +
  scale_color_manual(values = c("grey","#1c689e","grey","#1c689e")) + 
  theme(text=element_text(size=16,  family="serif"),legend.position = "nono") + xlab(NULL) + ylab(NULL) + 
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0)  
e + x11(width = 8,height = 6)
####Fig6D####
phylum <- read.delim('wjs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum <- data.frame(t(phylum))
group <- read.delim('group1.txt', sep = '\t', stringsAsFactors = FALSE)
library(mixOmics)
phylum <- phylum[group$names, ]
plsda_result <-plsda(phylum, group$group, ncomp = 3)
plsda_result_eig <- {plsda_result$explained_variance$X}[1:2]
sample_site <- data.frame(plsda_result$variates)[1:2]
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('plsda1', 'plsda2')
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)
write.table(sample_site, 'plsda234.txt', row.names = FALSE, sep = '\t', quote = FALSE)

plotIndiv(plsda_result, group = group$group, ellipse = TRUE, legend = TRUE, title = 'Faeces' ,ind.names = FALSE, style = 'ggplot2')

####figure6 EFGHI####
library(dplyr) 
library(ggplot2)
library(ggpubr)
compaired <- list(c("L","H"))
databar=read.csv(file='wjs_dx2.csv',header = T,stringsAsFactors = F)
e <- ggplot(databar, aes(x = dose, y = value,fill=dose)) +
  geom_boxplot(aes(color = dose), width = 0.7, size = 0.5,position = position_dodge(0.8),colour = "black",outlier.shape = NA) +
  geom_point(alpha = 0.6,colour = "black",shape = 21, size = 2, show.legend = F,position = position_jitterdodge(jitter.width = 0.2)) +
  theme(panel.grid = element_blank(), panel.background = element_blank()) + 
  theme_bw() + theme(panel.grid=element_blank()) +  facet_wrap(~group,scales="free",ncol=4) +
  scale_fill_manual(values = c("grey","#1c689e")) +
  scale_color_manual(values = c("grey","#1c689e")) + 
  theme(text=element_text(size=16,  family="serif"),legend.position = "nono") +  
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test,tip_length=0) 
e + x11(width = 8,height = 6)
