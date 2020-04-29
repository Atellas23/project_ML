library(MASS)
library(class)
library(dplyr)
library(klaR)

setwd(".")

red_w <- read.table("winequality-red.csv", sep=';', header = TRUE)
white_w <- read.table("winequality-white.csv", sep=';', header = TRUE)

red_w <- distinct(red_w)
white_w <- distinct(white_w)

wine <- rbind(red_w, white_w)

red_w <- red_w[c(-723,-964),]
white_w <- white_w[c(-2175, -2765),]
wine <- wine[c(-723,-964),]

wine <- rbind(red_w, white_w)
wine$type <- c(rep('r', nrow(red_w)), rep('w', nrow(white_w)))
wine.quality <- wine$quality
wine.data <- subset(wine, select = 1:11)

N <- nrow(wine)
neighbours <- 1:sqrt(N)

loop.k <- function(mydata, mytargets, myneighbours)
{
  errors <- matrix(nrow=length(myneighbours), ncol=2)
  colnames(errors) <- c("k","LOOCV error")
  
  for (k in myneighbours)
  {
    myknn.cv <- knn.cv(mydata, mytargets, k = myneighbours[k])
    
    # fill in number of neighbours and LOOCV error
    errors[k, "k"] <- myneighbours[k]
    
    tab <- table(Truth=mytargets, Preds=myknn.cv)
    errors[k, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
  }
  errors
}

set.seed(123)
loocv.error <- loop.k(scale(wine.data), wine$type, neighbours)
plot(loocv.error, type="l", xaxt = "n", main = "kNN Cross-Validation error with respect to k")
axis(1, neighbours)
which.min(loocv.error[,2])