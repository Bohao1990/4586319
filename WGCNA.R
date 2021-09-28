rm(list=ls())
options(stringsAsFactors = FALSE)
suppressMessages(library(GO.db))
suppressMessages(library(WGCNA))
suppressMessages(library(reshape2))
suppressMessages(library(stringr))

## 打开多线程
enableWGCNAThreads()
exprMat <- "expdata.txt"

## 相关性计算类型设置
type = "unsigned"
corType = "pearson"
corFnc = ifelse(corType=="pearson", cor, bicor)
## 对二元变量，如样本性状信息计算相关性时，或基因表达严重依赖于疾病状态时，需设置下面参数
maxPOutliers = ifelse(corType=="pearson",1,0.05)
## 关联样品性状的二元变量时，设置
robustY = ifelse(corType=="pearson",T,F)
#创建文件夹
dir.create("0.raw_data")
dir.create("1.filter")
dir.create("2.module_construction")
dir.create("3.modules")
dir.create("4.cordata")

##导入数据
dataExpr <- read.table(exprMat, sep='\t', row.names=1, header=T, 
                       quote="", comment="", check.names=F)
dim(dataExpr)
head(dataExpr)[,1:8]
write.csv(dataExpr,"./1.filter/dataExpr.csv")
a<-read.csv("./1.filter/dataExpr.csv",header = T)

## 数据筛选
# 筛选中位绝对偏差前75%的基因，至少MAD大于0.01
# (筛选后会降低运算量，也会失去部分信息也可不做筛选，使MAD大于0即可)
m.mad <- apply(dataExpr,1,mad)
dataExprVar <- dataExpr[which(m.mad > 
                                max(quantile(m.mad, probs=seq(0, 1, 0.25))[2],0.05)),]
dim(dataExprVar)
write.csv(dataExprVar,"./1.filter/dataExprVar.csv")
b<-read.csv("./1.filter/dataExprVar.csv",header = T)

suppressMessages(library(dplyr))
filter.genes<-filter(a,!X %in% b$X)
write.csv(filter.genes,"./1.filter/filter.genes.csv")

## 转换为样品在行，基因在列的矩阵
dataExpr <- as.data.frame(t(dataExprVar))

## 检测缺失值
gsg = goodSamplesGenes(dataExpr, verbose = 3)

if (!gsg$allOK){
  # Optionally, print the gene and sample names that were removed:
  if (sum(!gsg$goodGenes)>0) 
    printFlush(paste("Removing genes:", 
                     paste(names(dataExpr)[!gsg$goodGenes], collapse = ",")));
  if (sum(!gsg$goodSamples)>0) 
    printFlush(paste("Removing samples:", 
                     paste(rownames(dataExpr)[!gsg$goodSamples], collapse = ",")));
  # Remove the offending genes and samples from the data:
  dataExpr = dataExpr[gsg$goodSamples, gsg$goodGenes]
}

nGenes = ncol(dataExpr)
nSamples = nrow(dataExpr)
dim(dataExpr)
head(dataExpr)[,1:8]

## 查看是否有离群样品
pdf("./2.module_construction/phclust.pdf",width=48,height=36)
sampleTree = hclust(dist(dataExpr), method = "average")
par(cex = 0.92)
par(mar = c(5,12,10,6))
plot(sampleTree, main = "Sample clustering to detect outliers",
     sub="",xlab="",cex.lab=5,cex.axis=3,cex.main=7) 
abline(h = 80000, col = "red")
dev.off()

## 样本层级聚类和检测离群值
pdf("./2.module_construction/phclust-Z.pdf",width=48,height=36)
A = adjacency(t(dataExpr), type = "distance")
k = as.numeric(apply(A, 2, sum)) - 1
Z.k = scale(k)
thresholdZ.k = -5 
outlierColor = ifelse(Z.k < thresholdZ.k, "green", "black")
sampleTree = hclust(as.dist(1 - A), method = "average")
par(cex = 0.92)
par(mar = c(5,12,10,6))
plotDendroAndColors(sampleTree, groupLabels = names(outlierColor), 
                    colors = outlierColor, cex.lab=5,cex.axis=3,
                    cex.main=7,main = "Sample dendrogram and trait heatmap")
dev.off()

## 剔除离群样本
clust = cutreeStatic(sampleTree, cutHeight = 160, minSize = 10)
table(clust)
keepSamples = (clust==1)
dataExpr = dataExpr[keepSamples, ]
nGenes = ncol(dataExpr)
nSamples = nrow(dataExpr)
save(dataExpr, file = "FPKM-01-dataInput.RData")


## 软阈值筛选
powers = c(c(1:10), seq(from = 12, to=30, by=2))
sft = pickSoftThreshold(dataExpr, powerVector=powers, 
                        networkType=type, verbose=5)

pdf("./2.module_construction/soft_power.pdf",width=10,height=6)
par(mfrow = c(1,2))
cex1 = 0.9

# 横轴是Soft threshold (power)，纵轴是无标度网络的评估参数，数值越高，
# 网络越符合无标度特征 (non-scale)
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",
     ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red")
# 筛选标准。R-square=0.85
abline(h=0.85,col="red")

# Soft threshold与平均连通性
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, 
     cex=cex1, col="red")
dev.off()

power = sft$powerEstimate
power

## 无满足条件的power时选用
# 无向网络在power小于15或有向网络power小于30内，没有一个power值可以使
# 无标度网络图谱结构R^2达到0.8，平均连接度较高如在100以上，可能是由于
# 部分样品与其他样品差别太大。这可能由批次效应、样品异质性或实验条件对
# 表达影响太大等造成。可以通过绘制样品聚类查看分组信息和有无异常样品。
# 如果这确实是由有意义的生物变化引起的，也可以使用下面的经验power值。
if (is.na(power)){
  power = ifelse(nSamples<20, ifelse(type == "unsigned", 9, 18),
                 ifelse(nSamples<30, ifelse(type == "unsigned", 8, 16),
                        ifelse(nSamples<40, ifelse(type == "unsigned", 7, 14),
                               ifelse(type == "unsigned", 6, 12))       
                 )
  )
}

##一步法网络构建：One-step network construction and module detection##
# power: 上一步计算的软阈值
# maxBlockSize: 计算机能处理的最大模块的基因数量 (默认5000)；
#  4G内存电脑可处理8000-10000个，16G内存电脑可以处理2万个，32G内存电脑可
#  以处理3万个
#  计算资源允许的情况下最好放在一个block里面。
# corType: pearson or bicor
# numericLabels: 返回数字而不是颜色作为模块的名字，后面可以再转换为颜色
# saveTOMs：最耗费时间的计算，存储起来，供后续使用
# mergeCutHeight: 合并模块的阈值，越大模块越少
net = blockwiseModules(dataExpr, power = power, maxBlockSize = nGenes,
                       TOMType = type, minModuleSize = 50,
                       reassignThreshold = 0, mergeCutHeight = 0.15,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs=TRUE, corType = corType, 
                       maxPOutliers=maxPOutliers, loadTOMs=TRUE,
                       saveTOMFileBase = paste0(exprMat, ".tom"),
                       verbose = 3)

# 根据模块中基因数目的多少，降序排列，依次编号为 `1-最大模块数`。
# **0 (grey)**表示**未**分入任何模块的基因。 
table(net$colors)

## 模块聚类树绘制
# Convert labels to colors for plotting
moduleLabels = net$colors
moduleColors = labels2colors(moduleLabels)
# Plot the dendrogram and the module colors underneath
# 如果对结果不满意，还可以recutBlockwiseTrees，节省计算时间
pdf("./2.module_construction/ModuleTree.pdf",width=10,height=8)
plotDendroAndColors(net$dendrograms[[1]], moduleColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

dynamicColors <- labels2colors(net$unmergedColors)
plotDendroAndColors(net$dendrograms[[1]], cbind(dynamicColors,moduleColors),
                    c("Dynamic Tree Cut", "Module colors"),
                    dendroLabels = FALSE, hang = 0.5,
                    addGuide = TRUE, guideHang = 0.05)
dev.off()

## 共表达网络结果输出
gene_module <- data.frame(ID=colnames(dataExpr), module=moduleColors)
gene_module = gene_module[order(gene_module$module),]
write.table(gene_module,"./3.modules/gene_module.xls", sep="\t",quote=F)