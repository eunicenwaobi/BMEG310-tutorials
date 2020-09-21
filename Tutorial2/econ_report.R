library(ggplot2)
dat <-"https://raw.githubusercontent.com/bmeg310ubc/bmeg310/master/Tutorial%202/R%20Visualization/data/EconomistData.csv"
dat1 <- read.csv(dat)
#x-CPI // y-HDI
attach(dat1)
#question1
a <- ggplot(data=dat1, mapping=aes(x=CPI, y=HDI))+geom_point()
#question2
b <- ggplot(data=dat1, mapping=aes(x=CPI, y=HDI))+geom_point(color='#7bd3ff')
#question3
c <- ggplot(data=dat1, mapping=aes(x=CPI, y=HDI, color=as.factor(Region)))+geom_point()
#question4
d <- ggplot(data=dat1, mapping=aes(x=CPI, y=HDI, size=2))+geom_point()
#question5
e <- ggplot(data=dat1, mapping=aes(x=CPI, y=HDI, size=HDI.Rank))+geom_point()
