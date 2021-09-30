library(ggalluvial)
options(stringsAsFactors = F)
df <- read.table("ref.txt",sep = "\t",row.names = 1,header = T)
sapply(names(df),function(x) length(unique(df[,x])))
UCB_lodes <- to_lodes_form(df[,1:ncol(df)],
                           axes = 1:ncol(df),
                           id = "Cohort")
dim(UCB_lodes)
tail(UCB_lodes)
ggplot(UCB_lodes,
       aes(x = x,stratum = stratum, alluvium = Cohort, 
           fill = stratum, label = stratum))+
  scale_x_discrete(expand = c(0, 0))+
  geom_flow(width = 1/8)+
  geom_stratum(alpha = 0.9, width = 1/10)+
  geom_text(stat = "stratum", size = 3, color = "black")+
  xlab("")+ylab("")+theme_bw()+
  theme(panel.grid = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.line = element_blank(),axis.ticks = element_blank(),axis.text = element_blank())+
  ggtitle("")+
  guides(fill = F)
