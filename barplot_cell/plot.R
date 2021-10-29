library(ggplot2)
cellline<- read.table("cellline.txt",header = T,sep="\t")

cellline$Group=factor(cellline$Title,
                     levels=c("Normal-Control-1","Normal-Control-2",
                              "MDA-MB-436-1","MDA-MB-436-2",
                              "HCC1954-1","HCC1954-2"))
col <- c('Tomato','OrangeRed','DimGray')

ggplot(cellline, aes(x=Title, y=Value,fill=Cell_Lines))+
         geom_bar(stat = "identity", width = 0.6, position = position_dodge(0.8))+
         theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
         scale_fill_manual(values = col)
