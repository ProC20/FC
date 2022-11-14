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