library(DESeq2)
library(pheatmap)
library(ggplot2)
library(AnnotationDbi)
library(org.Hs.eg.db)
library(pathview)
library(gage)
library(gageData)
ftcounts <- "https://raw.githubusercontent.com/bmeg310ubc/bmeg310/master/Tutorial%207/GSE37704_featurecounts.csv"
metadata <- "https://raw.githubusercontent.com/bmeg310ubc/bmeg310/master/Tutorial%207/GSE37704_metadata.csv"
colData <- read.csv(metadata, row.names=1)
countData <- read.csv(ftcounts,row.names=1)
countData <- as.matrix(countData) 
countData = countData[,-1]
countData = countData[rowSums(countData)>1, ]
head(countData)

dds = DESeqDataSetFromMatrix(countData=countData,
                             colData=colData,
                             design=~condition)
dds = DESeq(dds)
res = results(dds, contrast=c("condition", "hoxa1_kd", "control_sirna"))
mcols(res, use.names = TRUE)
q1 <- as.numeric(sum(res$padj < 0.03, na.rm=TRUE))

genes.03<-sum(!is.na(res$pvalue))*0.03
pval.03 <-as.numeric(sum(res$pvalue < 0.03, na.rm=TRUE))  
fp <- 100*(genes.03)/pval.03
q2 <- as.numeric(sum(res$padj < ((fp/100)-0.01), na.rm=TRUE))  


resSig <- subset(res, padj < 0.03)
q3_up<-rownames(resSig[ order( resSig$log2FoldChange, decreasing=TRUE ), ])[1:5]
resSig <- subset(res, padj < 0.03)
q3_down<-rownames(resSig[ order( resSig$log2FoldChange ), ])[1:5]


#gene annotation
#rows = gene names
#cols = sample names
columns(org.Hs.eg.db)
res$symbol = mapIds(org.Hs.eg.db,
                    keys=row.names(res), 
                    column="SYMBOL",
                    keytype="ENSEMBL",
                    multiVals="first")
res$entrez = mapIds(org.Hs.eg.db,
                    keys=row.names(res), 
                    column="ENTREZID",
                    keytype="ENSEMBL",
                    multiVals="first")
res$name =   mapIds(org.Hs.eg.db,
                    keys=row.names(res), 
                    column="GENENAME",
                    keytype="ENSEMBL",
                    multiVals="first")

rws <- append(q3_up, q3_down, after=length(q3_up))


#mtx <- as.matrix(res[rws,1:6])

cls <- as.data.frame(res[rws,]$symbol)
colnames(cls) <- c("cols")
cls <- cls$cols
colData <- read.csv(metadata, row.names=1)
countData <- read.csv(ftcounts,row.names=1)
countData <- as.matrix(countData) 
countData = countData[,-1]
countData = countData[rowSums(countData)>1, ]
head(countData)
downstf <- countData[q3_down,]
upstf <- countData[q3_up,]
combo <- rbind(downstf, upstf)

mxdist <- dist(t(combo), upper=TRUE)#dist(t(mtx), upper=TRUE)
mf <- as.matrix(mxdist)
q4<-pheatmap(mf, 
         clustering_distance_rows = mxdist,
         clustering_distance_cols = mxdist,
         cluster_rows=TRUE, 
         #show_rownames=TRUE,
         #cluster_cols=TRUE,
         #dimnames=list(cls,rownames(mtx)),
         labels_col=cls 
        # annotation_col=cls
)


data(kegg.sets.hs)
data(sigmet.idx.hs)
kegg.sets.hs = kegg.sets.hs[sigmet.idx.hs]
foldchanges = res$log2FoldChange
names(foldchanges) = res$entrez
head(foldchanges)
keggres = gage(foldchanges, gsets=kegg.sets.hs)
q5_top <- rownames(keggres$greater[1:6,])
q5_down <- rownames(keggres$less[1:6,])
#top upreg pathways

#top dwnreg pathways
