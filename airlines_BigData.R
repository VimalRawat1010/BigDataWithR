library(bigmemory)
library(biganalytics)
library(bigtabulate)


library(parallel)

setwd("/media/vimal/10b357a2-8127-40d9-ab03-3e38ea800ce2/BigData/")
x <- read.big.matrix("airline.csv", type="integer", header=TRUE , backingfile="airline.bin", 
                     descriptorfile="airline.desc", extraCols="Age")

class(x)

birthmonth  <-function(y) {
                  minYear  <- min(y[,'Year'], na.rm=TRUE)
                  these  <- which(y[,'Year']== minYear)
                  minMonth  <-min(y[these ,'Month'], na.rm=TRUE)
                  return(12*minYear + minMonth  - 1)
            }

x <- attach.big.matrix(dget('airline.desc'))
colnames(x) 
system.time(colmean(x, 1, na.rm=TRUE)) 


require(foreach)
require(doMC)
registerDoMC(cores=4)

probs <- c(0.9, 0.99, 0.999, 0.9999)
desc <-  describe(x)


# delays by hour of day. Takes 26.7 seconds. 
anshourofday <- 
foreach (i=seq(0, colmax(x,"CRSDepTime")-1, by=60), 
         .combine=cbind)%dopar%
         {
           x <- attach.big.matrix(desc)
           ind <- mwhich(x, "CRSDepTime", c(i, i+60), comps=c('ge', 'lt'))
           m <- cbind(probs, quantile(x[ind, "DepDelay"], probs=probs, na.rm=TRUE))
           colnames(m) <- c("Probabilites", "Quantiles")
           t(m)[2,]
         }

plot(anshourofday[4,], type="l", col="blue", ylim=c(0,1500))
lines(anshourofday[3,], type="l", col="green")
lines(anshourofday[2,], type="l", col="red")
lines(anshourofday[1,], type="l", col="black")



#### Calculate Each Plane’s Birthmonth the big Way
aircrafts  <- unique(x[,'TailNum'])
acStart  <-rep(0, length(aircrafts))

acindices  <- bigsplit(x, 'TailNum', splitcol=NA_real_)

acStart  <-sapply(acindices , function(i)
                  birthmonth(x[i, c('Year','Month'), drop=FALSE ]))


#### Same task using doMC
library(doMC)
registerDoMC(cores =2)
acStart  <- foreach(i=acindices, .combine=c)%dopar% 
            { 
              return (birthmonth(x[i, c('Year', 'Month'),drop=FALSE]))
              }


### Finally, Compute an Estimate of Age
x[,'Age'] <- x[,'Year'] * as.integer(12) + 
             x[,'Month'] - as.integer(acStart[x[,'TailNum']])


#############   Linear Models
####### ry to predict arrival delay arrival (ArrDelay) using Age + Year
require(biganalytics)
blm  <-biglm.big.matrix(ArrDelay ~ Age + Year ,data=x)


