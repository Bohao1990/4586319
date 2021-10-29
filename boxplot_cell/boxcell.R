library(ggstatsplot)
library(ggplot2)
library(ggpubr)

CNV<- read.table("data.txt",header = T,sep="\t")
colnames(CNV)

CNV$CNV=factor(CNV$CopyNumberAlterations,levels=c("DeepDeletion","Diploid",
                                                    "Amplification"))

ggplot(CNV, aes(CopyNumberAlterations, LINC00467..mRNA.expression..RNA.Seq.RPKM.,fill= CNV))+
  stat_boxplot(geom = "errorbar",width=0.15)+
  geom_boxplot(position = "dodge2",width=0.6,outlier.shape = NA)+
  scale_fill_manual(values =c("gray","gray","gray","gray"))+
  geom_jitter(shape=16, position=position_jitter(0.2),aes(color = CNV),size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x = "Alteration Type", y = 'LINC00467 expression',title = "")+
  stat_compare_means(label = "p",comparisons = combn(levels(CNV$CNV),2,simplify =F))+
  theme(legend.position='none')
