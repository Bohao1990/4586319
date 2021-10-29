
LINCEXP<- read.table("Data.txt",header = T,sep = "\t")
LINCEXP$Group=factor(LINCEXP$Group,levels=c("T","N"))

library(ggplot2)
library(ggpubr)
ggplot(LINCEXP, aes(Group, LINC00467.expression,fill= Group))+
  stat_boxplot(geom = "errorbar",width=0.15)+
  geom_boxplot(aes(color = Group),position = "dodge2",width=0.7)+
  facet_wrap(~Probe.ID, nrow = 1) + scale_color_manual(values =c("black","black") ) +
  stat_compare_means(label = "p.signif", method = "t.test",label.y = 1.98,label.x = 1.42) + 
  scale_fill_manual(values =c("Tomato","DimGray"))+
 # geom_dotplot(binaxis='y', stackdir='center', dotsize = 0.35, fill = "black")+
  geom_jitter(shape=16, position=position_jitter(0.2),size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  stat_compare_means(comparisons = c("T","N"))+
  labs(x = "Type", y = 'LINC00467 expression',title = "")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_segment(aes(x=1, y=1.75, xend=1, yend=2))+  #绘制control端的竖线 
  geom_segment(aes(x=2, y=-0.28, xend=2, yend=2))+  #绘制treatment端竖线 
  geom_segment(aes(x=1, y=2, xend=1.26, yend=2))+ 
  geom_segment(aes(x=1.69, y=2, xend=2, yend=2))  #绘制两段横线
