alter <- read.table("fiho.txt",header = T,sep="\t")
colnames(alter)

ggplot(alter,aes(x=Cancer.Type, y=Alteration.Frequency,fill=Cancer.Type))+
  geom_bar(stat = "identity",width = 0.5, position = position_dodge(0.8))+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x = "Cancer Type", y = 'Alteration Frequency (%)',title = "")+
  geom_text(aes(label=X), 
            color="black", size=3.2,position=position_dodge(0.5),vjust=-0.5)+
  guides(fill=FALSE)+theme(axis.text.x = element_text(angle=45,hjust = 0.45,vjust = 0.48))

