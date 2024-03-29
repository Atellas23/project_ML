#' ---
#' title: "wine project"
#' author: "Àlex, Luis, Aleix"
#' date: "25/4/2020"
#' output: pdf_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(MASS)
library(class)
library(dplyr)
library(klaR)
library(magrittr)
library(caret)
library(randomForest)
library(biotools)
library(neural)
library(cclust)
library(nnet)

#' 
#' 
#' 
## ------------------------------------------------------------------------
our_seed <- 46290
red_w <- read.table("winequality-red.csv", sep=';', header = TRUE)
white_w <- read.table("winequality-white.csv", sep=';', header = TRUE)

#' 
#' 
## ------------------------------------------------------------------------
head(red_w)
dim(red_w)
head(white_w)
dim(white_w)

#' 
## ------------------------------------------------------------------------
red_w <- distinct(red_w)
white_w <- distinct(white_w)

wine <- rbind(red_w, white_w)

red_w <- red_w[c(-723,-964),]
white_w <- white_w[c(-2175, -2765),]
wine <- wine[c(-723,-964),]

#' 
## ------------------------------------------------------------------------
wine <- rbind(red_w, white_w)
wine$type <- c(rep('r', nrow(red_w)), rep('w', nrow(white_w)))
wine$type <- as.factor(wine$type)
wine$quality <- as.numeric(wine$quality)
wine.quality <- wine$quality
wine.data <- subset(wine, select = 1:11)

#' 
#' # ----------------------------------------------------------------------------------------
#' 
#' Separate data into test and training
## ------------------------------------------------------------------------
# Randomly separate into learning data and test data
set.seed(our_seed)
sample_size <- floor(2/3 * nrow(wine))
training_indices <- sample(seq_len(nrow(wine)), size = sample_size)
training.data  <- wine[training_indices, ]
testing.data <- wine[-training_indices, ]
training.red <- training.data[training.data$type == "r",]
training.white <- training.data[training.data$type == "w",]

#' # ----------------------------------------------------------------------------------------
#' 
#' ## TYPE CLASSIFICATION
#' 
#' ### QDA/RDA/LDA
#' #### Covariance matrix equality?
#' 
## ------------------------------------------------------------------------
boxM(training.data[,1:12], training.data$type)

#' --> not equal, hence QDA is supposed to be a good set of discriminant functions.
#' 
#' #### QDA
## ------------------------------------------------------------------------
qda.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  cv.err <- NULL
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    qda.pred <- qda(fmla, data = adj.data) %>% predict(newdata = val)
    predicted.class <- qda.pred$class
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- qda.kfcv(target = "type", training.data, nfolds = 10))
(QDA.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' We test if RDA works any better:
#' 
#' #### RDA
## ------------------------------------------------------------------------
rda.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  cv.err <- NULL
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    rda.pred <- rda(fmla, data = adj.data) %>% predict(newdata = val)
    predicted.class <- rda.pred$class
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- rda.kfcv(target = "type", training.data, nfolds = 10))
(RDA.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))


#' 
#' In case any of LDA or QDA don't work, we fit a RDA model and analyze its regularization parameters:
## ------------------------------------------------------------------------
set.seed(our_seed)
rda.model.10cv <- rda(x=training.data[,1:11], grouping=training.data$type, fold=10)
rda.model.10cv$regularization

#' We can see how lambda is very close to one and gamma is very close to zero, so we should just use LDA.
#' 
#' #### LDA
## ------------------------------------------------------------------------
lda.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  cv.err <- NULL
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    lda.pred <- lda(fmla, data = adj.data) %>% predict(newdata = val)
    predicted.class <- lda.pred$class
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- lda.kfcv(target = "type", training.data, nfolds = 10))
(LDA.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))


#lda.model <- lda(type ~ .-quality, data = training.data)
#loadings.lda <- as.matrix(training.data[,1:11]) %*% as.matrix(lda.model$scaling)
#plot(loadings.lda, col=c(training.data$type), ylab = "Projected coordinates", main = "LDA projection")

#' 
## ------------------------------------------------------------------------
#set.seed(our_seed)
#lda.model.loocv <- lda(x=wine.data, grouping=wine$type, CV=TRUE)
#(ct <- table(Truth=wine$type, Pred=lda.model.loocv$class))
#1 - sum(ct[row(ct)==col(ct)])/sum(ct)

#' 
## ------------------------------------------------------------------------
#set.seed(our_seed)
#qda.model.loocv <- qda(x=wine.data, grouping=wine$type, CV=TRUE)
#(ct <- table(Truth=wine$type, Pred=qda.model.loocv$class))
#1 - sum(ct[row(ct)==col(ct)])/sum(ct)

#' 
#' 
#' ### kNN
#' 
## ------------------------------------------------------------------------
loop.k <- function(mydata, mytargets, upper.bound)
{
  errors <- matrix(nrow=upper.bound, ncol=2)
  colnames(errors) <- c("k","LOOCV error")

  for (i in 1:upper.bound)
  {
    myknn.cv <- knn.cv(mydata, mytargets, k = i)
  
    # fill in number of neighbours and LOOCV error
    errors[i, "k"] <- i
  
    tab <- table(Truth=mytargets, Preds=myknn.cv)
    errors[i, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
  }
  errors
}

#' 
## ------------------------------------------------------------------------
# knn amb dades originals
set.seed(our_seed)
loocv.error <- loop.k(training.data[,1:11], training.data$type, sqrt(nrow(training.data)))
plot(loocv.error, type="l", xaxt = "n")
axis(1:sqrt(nrow(training.data)))
kmin.1 <- which.min(loocv.error[,2])
min1 <- loocv.error[kmin.1,]

# knn amb dades escalades
set.seed(our_seed)
loocv.error <- loop.k(scale(training.data[,1:11]), training.data$type, sqrt(nrow(training.data)))
plot(loocv.error, type="l", xaxt = "n")
axis(1:sqrt(nrow(training.data)))
kmin.2 <- which.min(loocv.error[,2])
min2 <- loocv.error[kmin.2,]

# knn amb dades projectades a l'espai d'LDA
set.seed(our_seed)
mod.lda <- lda(type~.-quality, data = training.data)
training.lda <- as.matrix(training.data[,1:11]) %*% as.matrix(mod.lda$scaling)
loocv.error.lda <- loop.k(training.lda, training.data$type, sqrt(nrow(training.data)))
plot(loocv.error.lda, type='l', xaxt='n')
axis(1:sqrt(nrow(training.data)))
kmin.3 <- which.min(loocv.error.lda[,2])
min3 <- loocv.error.lda[kmin.3,]

(list("minimum with original data" = min1, "minimum with scaled data" = min2, "minimum with LDA-projected data" = min3))

#' 
#' 
## ------------------------------------------------------------------------
knn.kfcv <- function(target, dd, num.neighbors, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    model <- lda(fmla, data = adj.data)
    adj.lda <- as.matrix(adj.data[,1:11]) %*% as.matrix(model$scaling)
    val.lda <- as.matrix(val[,1:11]) %*% as.matrix(model$scaling)
    predicted.class <- knn(train = adj.lda, test = val.lda, cl = adj.data[,target], k = num.neighbors)
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

#' 
#' 
## ------------------------------------------------------------------------
kfcv.err <- NULL
for (i in 1:25) {
  tab <- knn.kfcv(target="type", dd=training.data, num.neighbors=i)
  kNN.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
  kfcv.err <- c(kfcv.err, kNN.kfcv.err)
}
plot(c(1:25), kfcv.err, xlab = "number of neighbors")

(k.min <- which.min(kfcv.err))
kfcv.err[k.min]
# print(kfcv.err)

(tab <- knn.kfcv(target="type", dd=training.data, num.neighbors=5))
(kNN.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))


#' 
#' 
#' ### Logistic Regression
## ------------------------------------------------------------------------
set.seed(our_seed)
training.data$type <- factor(training.data$type)
k <- 10

glm.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  
  cv.err <- NULL
  t <- NULL
  p <- NULL
  
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    glm.pred <- glm(fmla, family = "binomial", data = adj.data) %>% predict(newdata = val, type = "response")
    predicted.class <- ifelse(glm.pred > 0.5, "w", "r")
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- glm.kfcv(target = "type", training.data, nfolds = 10))
(GLM.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' 
#' ### MLP
#' 
## ------------------------------------------------------------------------
set.seed(our_seed)
(decays <- 10^seq(-2, 0, by=0.2))

# we will train the model fixing a large number of units and using weight decay
mlp.model <- train(form = type ~ .-quality,
               data = training.data,
               trControl = trainControl(method = "CV", number = 10),
               method = "nnet",
               maxit = 200,
               tuneGrid = expand.grid(.size=20,.decay=decays),
               trace = FALSE)

d <- mlp.model$bestTune$decay

#' 
## ------------------------------------------------------------------------
mlp.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  
  cv.err <- NULL
  t <- NULL
  p <- NULL
  
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    
    mlp.pred <- nnet(fmla, data = adj.data, size = 20, decay = d, trace = FALSE) %>% predict(newdata = val)
    predicted.class <- ifelse(mlp.pred > 0.5, "w", "r")
    
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- mlp.kfcv(target = "type", training.data, nfolds = 10))
(MLP.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' ### Random Forest
#' 
## ------------------------------------------------------------------------
ntrees <- round(2^seq(1,10))
ntrees <- seq(150, 300, 10)


rf.results <- matrix (rep(0,2*length(ntrees)), nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0


ii <- 1
set.seed(our_seed)
training.data$type <- as.factor(training.data$type)
for (nt in ntrees)
{ 
  print(nt)
  
  # build forest
  model.rf <- randomForest(type ~ .-quality, data=training.data, ntree=nt, proximity=FALSE, 
                           sampsize=c(r=round(0.75*895), w=round(0.75*2649)), strata=training.data$type)
  
  # get the OOB and store it appropriately
  rf.results[ii, "OOB"] <- model.rf$err.rate[nt,1]  
  ii <- ii+1
}

rf.results

#' 
## ------------------------------------------------------------------------
rf.kfcv <- function(target, dd, nfolds = 10, seed = 46290, num_trees) {
  set.seed(seed)
  
  rownames(dd) <- NULL
  n <- nrow(dd)
  
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  
  cv.err <- NULL
  t <- NULL
  p <- NULL
  
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~.-quality"))
    
    model.rf1 <- randomForest (fmla, data=adj.data, ntree=num_trees, proximity=FALSE)
    pred.rf1 <- predict (model.rf1, val, type="class")
    
    t <- c(t, val[,target])
    p <- c(p, pred.rf1)
  }
  
  tab <- table(Truth = t, Pred = p)
}

(tab <- rf.kfcv("type", training.data, num_trees=290))
(rf.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' ### Type Classification Final Model
#' RBF has the lowest kfcv error, so it is the selected model to perform type classification. So, we calculate its (estimated) generalization error:
#' 
## ------------------------------------------------------------------------
set.seed(our_seed)
model <- lda(type~.-quality, data = training.data)
training.lda <- as.matrix(training.data[,1:11]) %*% as.matrix(model$scaling)
testing.lda <- as.matrix(testing.data[,1:11]) %*% as.matrix(model$scaling)
predicted.class <- knn(train = training.lda, test = testing.lda, cl = training.data$type, k = k.min)

(tab <- table(Truth = testing.data$type, Prediction = predicted.class))
(1 - sum(diag(tab))/sum(tab))*100

#' 
#' 
#' 
#' 
#' ## QUALITY PREDICTION
#' 
#' ### Multi-class Classification
#' 
## ------------------------------------------------------------------------
training.data$quality <- as.factor(training.data$quality)

lda(quality~., data = training.data)


#' 
#' 
#' #### RDA
## ------------------------------------------------------------------------
rda.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  cv.err <- NULL
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~."))
    rda.pred <- rda(fmla, data = adj.data) %>% predict(newdata = val)
    predicted.class <- rda.pred$class
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- rda.kfcv(target = "quality", training.data, nfolds = 10))
(RDA.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

set.seed(our_seed)
rda.model.10foldcv <- rda(quality~., data = training.data, crossval = TRUE, fold = 10)
rda.model.10foldcv$regularization

#' Again lambda = 1, gamma approx 0. Hence LDA looks alike:
#' 
#' #### LDA
## ------------------------------------------------------------------------
lda.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  cv.err <- NULL
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~."))
    lda.pred <- lda(fmla, data = adj.data) %>% predict(newdata = val)
    predicted.class <- lda.pred$class
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

(tab <- lda.kfcv(target = "quality", training.data[,1:12], nfolds = 10))
(LDA.kfcv.err <- (1 - sum(tab[row(tab)==col(tab)])/sum(tab))*100)
sum(tab)-sum(diag(tab))

#' 
#' #### kNN
#' 
## ------------------------------------------------------------------------
loop.k <- function(mydata, mytargets, upper.bound)
{
  errors <- matrix(nrow=upper.bound, ncol=2)
  colnames(errors) <- c("k","LOOCV error")

  for (i in 1:upper.bound)
  {
    myknn.cv <- knn.cv(mydata, mytargets, k = i)
  
    # fill in number of neighbours and LOOCV error
    errors[i, "k"] <- i
  
    tab <- table(Truth=mytargets, Preds=myknn.cv)
    errors[i, "LOOCV error"] <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab)
  }
  errors
}

#' 
## ------------------------------------------------------------------------
# knn amb dades originals
set.seed(our_seed)
loocv.error <- loop.k(training.data[,1:11], training.data$quality, sqrt(nrow(training.data)))
plot(loocv.error, type="l", xaxt = "n")
axis(1:sqrt(nrow(training.data)))
kmin.1 <- which.min(loocv.error[,2])
min1 <- loocv.error[kmin.1,]

# knn amb dades escalades
set.seed(our_seed)
loocv.error <- loop.k(scale(training.data[,1:11]), training.data$quality, sqrt(nrow(training.data)))
plot(loocv.error, type="l", xaxt = "n")
axis(1:sqrt(nrow(training.data)))
kmin.2 <- which.min(loocv.error[,2])
min2 <- loocv.error[kmin.2,]

# knn amb dades LD-projectades
set.seed(our_seed)
mod.lda <- lda(quality~.-type, data = training.data)
training.lda <- as.matrix(training.data[,1:11]) %*% as.matrix(mod.lda$scaling)
loocv.error.lda <- loop.k(training.lda, training.data$quality, sqrt(nrow(training.data)))
plot(loocv.error.lda, type='l', xaxt='n')
axis(1:sqrt(nrow(training.data)))
kmin.3 <- which.min(loocv.error.lda[,2])
min3 <- loocv.error.lda[kmin.3,]

(list("minimum with original data" = min1, "minimum with scaled data" = min2, "minimum with LDA-projected data" = min3))

#' 
## ------------------------------------------------------------------------
knn.lda.kfcv <- function(target, dd, num.neighbors, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,]
    val <- dd[fold,]
    fmla <- as.formula(paste(target,"~."))
    model <- lda(fmla, data = adj.data)
    adj.lda <- as.matrix(adj.data[,1:11]) %*% as.matrix(model$scaling)
    val.lda <- as.matrix(val[,1:11]) %*% as.matrix(model$scaling)
    predicted.class <- knn(train = adj.lda, test = val.lda, cl = adj.data[,target], k = num.neighbors)
    t <- c(t, val[,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

knn.scaled.kfcv <- function(target, dd, num.neighbors, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- scale(dd[-fold,1:11])
    adj.class <- dd[-fold,target]
    val <- scale(dd[fold,1:11])
    predicted.class <- knn(train = adj.data, test = val, cl = adj.class, k = num.neighbors)
    t <- c(t, dd[fold,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

knn.original.kfcv <- function(target, dd, num.neighbors, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- dd[-fold,1:11]
    adj.class <- dd[-fold,target]
    val <- dd[fold,1:11]
    predicted.class <- knn(train = adj.data, test = val, cl = adj.class, k = num.neighbors)
    t <- c(t, dd[fold,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

#' 
#' 
#' 
## ------------------------------------------------------------------------
# LDA projected data
kfcv.err <- NULL
for (i in 1:59) {
  tab <- knn.lda.kfcv(target = "quality", dd = training.data[,-13], num.neighbors = i)
  kNN.kfcv.err <- 1 - sum(tab[row(tab) == col(tab)])/sum(tab)
  kfcv.err <- c(kfcv.err, kNN.kfcv.err)
}
plot(c(1:59), kfcv.err, xlab = "number of neighbors")
(k.min <- which.min(kfcv.err))
kfcv.err[k.min]*100

## ------------------------------------------------------------------------
# scaled data
kfcv.err <- NULL
for (i in 1:59) {
  tab <- knn.scaled.kfcv(target = "quality", dd = training.data[,-13], num.neighbors = i)
  kNN.kfcv.err <- 1 - sum(tab[row(tab) == col(tab)])/sum(tab)
  kfcv.err <- c(kfcv.err, kNN.kfcv.err)
}
plot(c(1:59), kfcv.err, xlab = "number of neighbors")
(k.min <- which.min(kfcv.err))
kfcv.err[k.min]*100

#' 
## ------------------------------------------------------------------------
# original scale data
kfcv.err <- NULL
for (i in 1:59) {
  tab <- knn.original.kfcv(target = "quality", dd = training.data[,-13], num.neighbors = i)
  kNN.kfcv.err <- 1 - sum(tab[row(tab) == col(tab)])/sum(tab)
  kfcv.err <- c(kfcv.err, kNN.kfcv.err)
}
plot(c(1:59), kfcv.err, xlab = "number of neighbors")
(k.min <- which.min(kfcv.err))
kfcv.err[k.min]*100

#' 
#' 
#' #### Ordinal Regression
## ------------------------------------------------------------------------
# set.seed(our_seed)
# training.data$quality <- as.factor(training.data$quality)
# k <- 10
# 
# ordreg.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
#   set.seed(seed)
#   rownames(dd) <- NULL
#   n <- nrow(dd)
#   folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
#   
#   cv.err <- NULL
#   t <- NULL
#   p <- NULL
#   
#   for (fold in folds) {
#     adj.data <- dd[-fold,]
#     val <- dd[fold,]
#     fmla <- as.formula(paste(target,"~."))
#     predicted.class <- polr(fmla, data = adj.data) %>% predict(newdata = val)
#     t <- c(t, val[,target])
#     p <- c(p, predicted.class)
#   }
#   tab <- table(Truth = t, Pred = p)
# }
# 
# (tab <- ordreg.kfcv(target = "quality", training.data, nfolds = 10))
# (ordreg.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' 
#' ### Regression
#' 
## ------------------------------------------------------------------------
training.data$quality <- as.numeric(training.data$quality)

#' 
#' #### Linear Regression
#' ##### Unregularized
## ------------------------------------------------------------------------
set.seed(our_seed)
training.data$quality <- as.numeric(training.data$quality)
training.data$type <- as.factor(training.data$type)

k <- 10
quality.lm.model <- train(form = quality ~ .,
               data = training.data,
               trControl = trainControl(method = "cv", number = k),
               method = "lm")

# Table of results along with accuracy
preds <- round(predict(quality.lm.model))
(linearRegression.kfcv.err <- mean((preds - training.data$quality)^2))

#' 
#' ##### Ridge (L^2-regularized)
#' 
## ------------------------------------------------------------------------
lambdas <- seq(28,29,0.001)

model.ridge <- lm.ridge(training.data$quality~., data = as.data.frame(scale(training.data[,1:11])), lambda = lambdas)
plot(lambdas, model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by
(lambda.ridge <- lambdas[which.min(model.ridge$GCV)])
abline (v=lambda.ridge,lty=2)

#' 
## ------------------------------------------------------------------------
training.data$type <- as.factor(training.data$type)
ridge.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
  set.seed(seed)
  rownames(dd) <- NULL
  n <- nrow(dd)
  folds <- createFolds(seq_len(n), k = nfolds, list = TRUE)
  cv.err <- NULL
  t <- NULL
  p <- NULL
  for (fold in folds) {
    adj.data <- as.data.frame(scale(dd[-fold,]))
    val <- scale(dd[fold,])
    ridge.mod <- lm.ridge(adj.data$quality~., data = adj.data, lambda = lambda.ridge)
    beta.ridge <- coef(ridge.mod)
    temp.ridge.pred <- beta.ridge[1] + as.matrix(val[,1:11]) %*% beta.ridge[2:12]
    ridge.pred <- round(temp.ridge.pred)
    t <- c(t, val[,target])
    p <- c(p, ridge.pred)
  }
  mse <- mean((t-p)^2)
}

ridge.kfcv.err.w <- ridge.kfcv("quality", training.white[,-13])
ridge.kfcv.err.r <- ridge.kfcv("quality", training.red[,-13])

(ridge.kfcv.err <- (nrow(training.white)*ridge.kfcv.err.w + nrow(training.red)*ridge.kfcv.err.r)/(nrow(training.red)+nrow(training.white)))

lambda.ridge

#' 
#' 
#' ##### LASSO (L^1-regularized)
#' 
## ------------------------------------------------------------------------
library(Matrix)
library(glmnet)

t <- as.matrix(scale(training.data[,1:11]), ncol = 11)

model.lasso <- cv.glmnet(t, as.numeric(training.data$quality), nfolds = 10)

(lambda.lasso <- model.lasso$lambda.min)

plot(model.lasso)
(LASSO.kfcv.err <- model.lasso$cvm[model.lasso$lambda == lambda.lasso])

#' 
