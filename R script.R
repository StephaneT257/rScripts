file <- read.csv("C:\\Users\\steph\\Downloads\\filesize.csv")
#Just checking head
#head(file)
#Q1
#Hist of
hist(file$x, breaks= 50 , main = "Histogram of filesize", xlab = "size in KB", ylab = "frequency")
#Mean
mean <-mean(file$x)
print(mean)
#SD
SD <-sd(file$x)
print(SD)
#Median
median <- median(file$x)
print(median)
#first quartile
quart1 <-quantile(file$x,0.25)
print(quart1)
#third quartile
quart3 <-quantile(file$x,0.75)
print(quart3)
#Start of MLE Q2
n <- 1000
# using formula alpha hat = n/ sum from i to n of nat log of (x) -nln(k)
# n/sum of nat log of x (g) - nln(k) (h)
h <- log(1000)
#Start of MLE Q2
n <- 1000
# using formula alpha hat = n/ sum from i to n of nat log of (x) -nln(k)
# n/sum of nat log of x (g) - nln(k) (h)
h <- log(1000)
h <-h*1000
g <- 0
for( i in file$x ) {
  g <- g+ log(i)
}
mle <- n/(g-h)
install.packages('EnvStats')
install.packages('Pareto')
library(EnvStats)
library(Pareto)
simpareto <- rpareto(1000,1000,2.793)
YVar <-(1/1000)*simpareto
hist(YVar, breaks= 20 , main = "Histogram of simulation", xlab = "size in KB", ylab = "frequency")
#Mean
meanVAR <-mean(YVar)
print(meanVAR)
#SD
SDVAR <-sd(YVar)
print(SDVAR)
#Median
medianVAR <- median(YVar)
print(medianVAR)
#first quartile
quart1VAR <-quantile(YVar,0.25)
print(quart1VAR)
#third quartile
quart3VAR <-quantile(YVar,0.75)
print(quart3VAR)
#Simulate with 1000000, just very large
millsample <- rPareto(1000000, 1000, 2.793)
#Find the 99% quantile of it
quantile99 <-quantile(millsample,0.99)
print(quantile99)
