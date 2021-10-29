library(survival)
library(survminer)
library(ggplot2)
library(cowplot)

P_OS=read.csv("Pancanecer Overall.csv")
colnames(P_OS)

fit=survfit(Surv(Time..months.,Status)~Group,data=P_OS)
ggsurvplot(fit, conf.int=T, risk.table=T, xlab = "Time(Months)",xlim=c(0,60),
           legend.labs=c("Altered","Unaltered "),legend.title="Group", ylim=c(0.6,1),
           ggtheme=theme_bw()+
           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 plot.title = element_text(hjust = 0.5)),
           title="Overall Survival", break.time.by=10, 
           pval.coord=c(2,0.62), risk.table.y.text=F,
           pval=TRUE,
           risk.table.height=.26)


P_DSS=read.csv("PanCancer Disease-specific_.csv")
colnames(P_DSS)

fit=survfit(Surv(Time..months.,Status)~Group,data=P_DSS)

ggsurvplot(fit, conf.int=T,risk.table=T, xlab = "Time(Months)",xlim=c(0,60),
               legend.labs=c("Altered","Unaltered "),legend.title="Group", ylim=c(0.6,1),
               ggtheme=theme_bw()+
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust = 0.5)),
               title="Disease specific Survival", break.time.by=10,
               pval=TRUE,
               pval.coord=c(2,0.62), risk.table.y.text=F,
               risk.table.height=.26)


P_DFS=read.csv("Pancancer Disease_Free.csv")
colnames(P_DFS)

fit=survfit(Surv(Time..months.,Status)~Group,data=P_DFS)

ggsurvplot(fit, conf.int=T,risk.table=T, xlab = "Time(Months)",xlim=c(0,60),
           legend.labs=c("Altered","Unaltered "),legend.title="Group", ylim=c(0.5,1),
           ggtheme=theme_bw()+
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   plot.title = element_text(hjust = 0.5)),
           title="Disease free Survival", break.time.by=10,
           pval=TRUE,
           pval.coord=c(2,0.62), risk.table.y.text=F,
           risk.table.height=.26)
 
 
P_PFS=read.csv("PanCancer Progression_Free.csv")
colnames(P_PFS)

fit=survfit(Surv(Time..months.,Status)~Group,data=P_PFS)

ggsurvplot(fit, conf.int=T,risk.table=T, xlab = "Time(Months)",xlim=c(0,60),
           legend.labs=c("Altered","Unaltered "),legend.title="Group", ylim=c(0.5,1),
           ggtheme=theme_bw()+
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   plot.title = element_text(hjust = 0.5)),
           title="Progression free Survival", break.time.by=10,
           pval=TRUE,
           pval.coord=c(2,0.62), risk.table.y.text=F,
           risk.table.height=.26)
