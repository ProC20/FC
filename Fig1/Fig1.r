####Fig 1B####
library(ggplot2)
library(ggsignif)
library(forcats)
library(ggsci)
df <- read.table("exp_1.txt",header = T,sep="\t",na.strings = "NA")
compaired <- list(c("CTRL","LOP"),c("LOP","PHE"),c("LOP","B. lon 4"),c("LOP","B. lon 70"),c("LOP","B. lon 8"),c("LOP","B. lon 39"),c("LOP","B. lon 6"))  
df$group <- fct_inorder(df$group)
ggplot(df, aes(y=value, x=group))+ facet_wrap(~index,scales="free") + 
  geom_bar(aes(fill=group), position=position_dodge(1),stat="summary",width=0.7,colour = "black")+
  theme_bw()+
  theme(legend.direction = "horizontal", legend.position = "none", axis.text.x = element_text(angle = 0))+
  labs(title = "", y=" ", x = "")+
  scale_y_continuous(expand = c(0,0))+
  geom_jitter(data = df,  aes(fill=group, y=value, x=group),size = 2, shape = 21)+
  stat_summary(fun.data = 'mean_sdl', geom = "errorbar", colour = "black",
               width = 0.2,position = position_dodge(1))+
  geom_signif(comparisons = compaired,step_increase = 0.1,map_signif_level = T,test = wilcox.test) + 
  scale_fill_manual(values = c('#db9a2f','#0b0b09','#5dabd3','#0e946a','#ece34a','#1c689e','#c45b2d','#bf7199')) +
scale_fill_manual(values = c('#db9a2f','#0b0b09','#5dabd3','#0e946a','#ece34a','#1c689e','#c45b2d','#bf7199'))