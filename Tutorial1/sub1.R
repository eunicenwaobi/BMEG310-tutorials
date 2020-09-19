#Author: Chukwuamaka Eunice Nwaobi
#Studentnumber:12710521
#Tutorial: BMEG 310 T1A
#Date: 2020/16/09

file="https://raw.githubusercontent.com/bmeg310ubc/bmeg310/master/Tutorial%201/2-%20R%20basics/data/GSE71562.csv"
val<-maxgene(file)
data <- read.csv(file, header=FALSE)

maxgene <- function(filestr){
  data <- read.csv(filestr, header=FALSE)
  trunc <- data.frame(t(data[2:ncol(data)]))
  trunc <- data.frame(t(trunc[2:nrow(data)]))
  trunc <- data.matrix(trunc)
  means <- apply(trunc,2, mean)
  means <-as.vector(colMeans(trunc), mode="numeric")
  
  hval <- data[1,][2:ncol(data)]
  mxidx<-as.numeric(which(means == max(means)))
  hval <- hval[mxidx]
  h<-max(means)
  sde <- sd(means)
  m <- mean(means)
  idx <- which(means-(2*sde+m) >= 0 )
  above2sd <- data[1, idx]
  print(h) #prints highest gene number
  print(hval) #prints highest gene name 
  print(ncol(above2sd)) #prints number of genes 2 sds above mean of means
  print(above2sd) #prints genes 2sds above mean of means
  return(hval)
}
