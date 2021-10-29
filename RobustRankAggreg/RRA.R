setwd("C:\\Users\\Administrator\\Desktop\\LINC00467 BRAC GEO2R\\RRA\\DE-lncRNAs")

## GSE数据集差异表达基因（按logFC值排序）并为一个list，正序倒序各一个list
padj=0.05
logFC=1
files=c("GSE7904.txt","GSE45827.txt","GSE65194.txt","GSE22820.txt","GSE38959.txt")
upList=list()
downList=list()
allFCList=list()
for(i in 1:length(files)){
  inputFile=files[i]
  rt=read.table(inputFile,header=T,sep = '\t',quote = '') # 注意文件读取
  header=unlist(strsplit(inputFile,"_"))
  downList[[header[1]]]=as.vector(rt[,1])
  upList[[header[1]]]=rev(as.vector(rt[,1]))
  fcCol=rt[,1:2]
  colnames(fcCol)=c("Gene",header[[1]])
  allFCList[[header[1]]]=fcCol
}

## 构建差异基因在GSE数据集中logFC矩阵
mergeLe=function(x,y){
  merge(x,y,by="Gene",all=T)}
newTab=Reduce(mergeLe,allFCList)
rownames(newTab)=newTab[,1]
newTab=newTab[,2:ncol(newTab)]
newTab[is.na(newTab)]=0

## 筛选共同上调基因
library(RobustRankAggreg)
upMatrix = rankMatrix(upList)
upAR = aggregateRanks(rmat=upMatrix)
colnames(upAR)=c("Name","Pvalue")
upAdj=p.adjust(upAR$Pvalue,method="bonferroni")
upXls=cbind(upAR,adjPvalue=upAdj)
upFC=newTab[as.vector(upXls[,1]),]
upXls=cbind(upXls,logFC=rowMeans(upFC))
write.table(upXls,file="up.xls",sep="\t",quote=F,row.names=F)
upSig=upXls[(upXls$adjPvalue<padj & upXls$logFC>logFC),]
write.table(upSig,file="upSig.xls",sep="\t",quote=F,row.names=F)

## 筛选共同下调基因
downMatrix = rankMatrix(downList)
downAR = aggregateRanks(rmat=downMatrix)
colnames(downAR)=c("Name","Pvalue")
downAdj=p.adjust(downAR$Pvalue,method="bonferroni")
downXls=cbind(downAR,adjPvalue=downAdj)
downFC=newTab[as.vector(downXls[,1]),]
downXls=cbind(downXls,logFC=rowMeans(downFC))
write.table(downXls,file="down.xls",sep="\t",quote=F,row.names=F)
downSig=downXls[(downXls$adjPvalue<padj & downXls$logFC< -logFC),]
write.table(downSig,file="downSig.xls",sep="\t",quote=F,row.names=F)

## 合并共同上下调基因
allSig = rbind(upSig,downSig)
colnames(allSig)
allSig = allSig[,c("Name","logFC")]
write.table(allSig,file = 'allSign.xls',sep = '\t',quote = F)

## 绘制logFC的热图
hminput=newTab[c(as.vector(upXls[1:5,1]),as.vector(downXls[1:5,1])),]
library(pheatmap)
tiff(file="logFC.jpg",width = 30,height = 20,units ="cm",compression="lzw",res=600)
pheatmap(hminput,display_numbers = TRUE,
         fontsize_row=10,
         fontsize_col=12,
         color = colorRampPalette(c("blue", "white", "red"))(50),
         cluster_cols = FALSE,cluster_rows = FALSE, )
dev.off()
