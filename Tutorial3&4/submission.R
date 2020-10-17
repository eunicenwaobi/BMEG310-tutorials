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
mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))
sub2 <- ggbiplot(mtcars.pca,ellipse=TRUE, labels=rownames(mtcars), groups=mtcars.country, var.axes = FALSE)+ggtitle("MTCARS and PCA Arranged by Countries")+xlab("Std. PC1")+ylab("Std. PC2")

