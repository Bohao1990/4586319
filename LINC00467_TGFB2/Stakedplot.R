perdata<- read.table("data.txt",header = T,sep = "\t")

library(ggplot2)
library(ggpubr)

ggplot(data=perdata, mapping=aes(x=Group,y= Percent, fill=TGFB2))+ 
  geom_bar(aes(fill = TGFB2), stat = "identity",width=0.5)+
  labs(x = 'Group', y = 'Sample Percent (%)') +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(y_position = 117, xmin = 1, xmax = 2, 
              annotations = "p<0.0001",tip_length = 0.17)+ 
  geom_text(aes(label=Ratio),size=2.5,position=position_stack(0.5))
