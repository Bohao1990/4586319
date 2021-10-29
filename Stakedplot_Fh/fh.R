perdata<- read.table("DATA.txt",header = T,sep = "\t")

library(ggplot2)
library(ggpubr)

ggplot(data=perdata, mapping=aes(x=Group,y= Percent, fill=T.stage))+ 
  geom_bar(aes(fill = T.stage), stat = "identity",width=0.5)+
  labs(x = 'Group', y = 'Stage Percent (%)') +
  theme_bw()+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(y_position = 110, xmin = 1, xmax = 2, 
              annotations = "p=0.006",tip_length = 0.17)+ 
  geom_text(aes(label=Ratio),size=2.5,position=position_stack(0.5))
