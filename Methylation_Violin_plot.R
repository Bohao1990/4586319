library(ggplot2)
library(ggpubr)
data<- read.table("data.txt",header = T,sep = "\t")
ggplot(data, aes(x=Group,y=LINC00467.Methylation,fill= Group))+
  geom_violin(position = position_dodge(width = 0.8), scale= "area",width=0.6)+
  geom_boxplot(position = position_dodge(width = 0.8),width=0.2,show.legend = F,outlier.size = 0,fill = "orange")+
  scale_fill_manual(values = c("blue","red"))+ylab("LINC00467 Methylation")+
  theme_bw()+theme(panel.grid = element_line(color = "white"))+
  stat_compare_means(label = "p.format", method = "t.test",label.y = 0.42,label.x = 1.37)+
  geom_segment(aes(x=1, y=0.24, xend=1, yend=0.4))+  
  geom_segment(aes(x=2, y=0.39, xend=2, yend=0.4))+   
  geom_segment(aes(x=1, y=0.4, xend=2, yend=0.4))