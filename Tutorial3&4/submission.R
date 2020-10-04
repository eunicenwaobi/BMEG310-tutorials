library(readxl)
library(remotes)
library(ggbiplot)
library(corrplot)
library(caret)
library(ggplot2)
suppressPackageStartupMessages(library(dplyr))
set.seed(786)

##PCA - 2
mtcars.pca <- prcomp(mtcars[,c(1:7, 10, 11)], center=TRUE, scale.=TRUE)
#summary(mtcars.pca)
#str(mtcars.pca)
#ggbiplot(mtcars.pca)
#ggbiplot(mtcars.pca,choices=c(3,4))
#ggbiplot(mtcars.pca, labels=rownames(mtcars))
#categorized into US, Japanese, European
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
sub2 <- ggbiplot(mtcars.pca,ellipse=TRUE, labels=rownames(mtcars), groups=mtcars.country)+ggtitle("MTCARS and PCA Arranged by Countries")+xlab("Std. PC1")+ylab("Std. PC2")

