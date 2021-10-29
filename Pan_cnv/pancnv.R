library(ggplot2)
CNV<- read.table("plot-CNV-EXP.txt",header = T,sep="\t")

CNV$CNV=factor(CNV$CNV,levels=c("Shallow Deletion","Diploid",
                               "Gain","Amplification"))

ggplot(CNV, aes(CNV, EXP,fill= CNV))+
  stat_boxplot(geom = "errorbar",width=0.15)+
  geom_boxplot(position = "dodge2",width=0.6,outlier.shape = NA)+
  scale_fill_manual(values =c("gray","gray","gray","gray"))+
  geom_jitter(shape=16, position=position_jitter(0.2),aes(color = CNV),size=1)+
  theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x = "Alteration Type", y = 'LINC00467 expression',title = "")+
  stat_compare_means(label = "p",comparisons = combn(levels(CNV$CNV),2,simplify =F))+
  theme(legend.position='none')

CORDATA <- read.table("plot CORDATA.txt",header = T,sep="\t")
colnames(CORDATA)

library(ggstatsplot)
library(ggplot2)
set.seed(123)
ggscatterstats(
  data = CORDATA,
  x = LINC00467..mRNA.Expression..RSEM..Batch.normalized.from.Illumina.HiSeq_RNASeqV2.,
  y = LINC00467..Log2.copy.number.values,
  smooth.line.args = list(size = 1),
  point.args = list(size = 2, alpha = 0.3),
  xlab = "LINC00467 Expression",
  ylab = "LINC00467 Log2(CNV values)",
  xalpha = 0.6,
  yalpha = 0.6,
  title = "",
  marginal.type = "density",
  centrality.para = "median",
  centrality.label.args = list(size = 1.6),
  ggtheme = ggplot2::theme_bw()+theme(panel.grid = element_line(color = "white")),
  messages = FALSE
)
