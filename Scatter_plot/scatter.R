expdata<- read.table("Exp.txt", header = T, sep = "\t", quote = "")
methydata<- read.table("Methy.txt",header = T, sep = "\t", quote = "")
ATACDATA<- read.table("ATAC.txt",header = T, sep = "\t", quote = "")
mergedata<- merge(expdata,methydata,by="sample")

mergedata2<- merge(expdata,ATACDATA,by="sample")
  
library(ggstatsplot)
library(ggplot2)
set.seed(123)
ggscatterstats(
  data = mergedata,
  x = ENSG00000153363.11,
  y = LINC00467,
  smooth.line.args = list(size = 1, color = "orange"),
  point.args = list(size = 2, alpha = 0.3),
  xfill = "red",
  yfill = "blue",
  xlab = "LINC00467 Expression",
  ylab = "LINC00467 Methylation",
  xalpha = 0.6,
  yalpha = 0.6,
  title = "",
  marginal.type = "density",
  centrality.para = "median",
  centrality.label.args = list(size = 1.6),
  ggtheme = ggplot2::theme_bw()+theme(panel.grid = element_line(color = "white")),
  messages = FALSE
)

ggscatterstats(
  data = mergedata2,
  x = ENSG00000153363.11,
  y = LINC00467.ATAC,
  smooth.line.args = list(size = 1, color = "orange"),
  point.args = list(size = 2, alpha = 0.3),
  xfill = "red",
  yfill = "blue",
  xlab = "LINC00467 Expression",
  ylab = "ATAC Seq promoter peak signal of LINC00467",
  xalpha = 0.6,
  yalpha = 0.6,
  title = "",
  marginal.type = "density",
  centrality.para = "median",
  centrality.label.args = list(size = 1.6),
  ggtheme = ggplot2::theme_bw()+theme(panel.grid = element_line(color = "white")),
  messages = FALSE
)

