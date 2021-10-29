data<- read.table("data.txt",header = T,sep = "\t")
library(ggplot2) 
library(ggrepel) 
library(dplyr)
ggplot(data=data,aes(x=ES,y=-log10(pvalue)))+ 
  geom_point(data=subset(data,data$pvalue >= 0.05),aes(size=GeneCount),alpha=0.6)+ ## 画无差异的点
  geom_point(data=subset(data,data$pvalue < 0.05 & data$ES > 0.5),aes(size=GeneCount),color='red',alpha=0.6)+  ## 画出上调的点
  geom_point(data=subset(data,data$pvalue < 0.05 & data$ES < -0.5),aes(size=GeneCount),color='blue',alpha=0.6)+ ## 画下调的点
  geom_hline(yintercept = -log10(0.05),lty=4,lwd=0.6,alpha=0.8)+ ## 画竖线 
  geom_vline(xintercept = c(0.5,-0.5),lty=4,lwd=0.6,alpha=0.8)+ ## 画横线
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = 'black'))+ # 去除背景
  geom_text_repel(data=subset(data, abs(ES) > 0.5 & pvalue < 0.05),aes(label=ID),col='red',
                  alpha = 0.8,ylim = c(0.5,1.2), label.size = 0.3,
                  stat = "identity", position = "identity",
                  direction = c("both", "y", "x"), angle = 0, vjust = 0)
