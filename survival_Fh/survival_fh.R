library(survival)
library(survminer)
library(ggplot2)
library(cowplot)

F_OS=read.csv("Firehose Overall.csv")
colnames(F_OS)

fit=survfit(Surv(Time..months.,Status)~Group,data=F_OS)

ggsurvplot(fit, conf.int=T,risk.table=T, xlab = "Time(Months)",xlim=c(0,60),
           legend.labs=c("Altered","Unaltered "),legend.title="Group", ylim=c(0.6,1),
           ggtheme=theme_bw()+
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   plot.title = element_text(hjust = 0.5)),
           title="Overall Survival", break.time.by=10,
           pval=TRUE,
           pval.coord=c(2,0.62), risk.table.y.text=F,
           risk.table.height=.26)
