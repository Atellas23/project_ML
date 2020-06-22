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
training.red <- training.data[training.data$type == "r",]
training.white <- training.data[training.data$type == "w",]
testing.data <- wine[-training_indices, ]

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
plot(loocv.error, type="l", xaxt = "n", main = "kNN cv-error (original data)")
axis(1:sqrt(nrow(training.data)))
kmin.1 <- which.min(loocv.error[,2])
min1 <- loocv.error[kmin.1,]

# knn amb dades escalades
set.seed(our_seed)
loocv.error <- loop.k(scale(training.data[,1:11]), training.data$type, sqrt(nrow(training.data)))
plot(loocv.error, type="l", xaxt = "n", main = "kNN cv-error (scaled data)")
axis(1:sqrt(nrow(training.data)))
kmin.2 <- which.min(loocv.error[,2])
min2 <- loocv.error[kmin.2,]

# knn amb dades projectades a l'espai d'LDA
set.seed(our_seed)
mod.lda <- lda(type~.-quality, data = training.data)
training.lda <- as.matrix(training.data[,1:11]) %*% as.matrix(mod.lda$scaling)
loocv.error.lda <- loop.k(training.lda, training.data$type, sqrt(nrow(training.data)))
plot(loocv.error.lda, type='l', xaxt='n', main = "kNN cv-error (LDA-projected data)")
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

#' 
#' 
#' ### Logistic Regression
## ------------------------------------------------------------------------
set.seed(our_seed)
training.data$type <- factor(training.data$type)
k <- 10
glm_model <- train(form = type ~ .-quality,
               data = training.data,
               trControl = trainControl(method = "cv", number = k),
               method = "glm",
               family = "binomial")

# Table of results along with misclassification rate
(tab <- table(training.data$type, predict(glm_model)))
(glm.kfcv.err <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' 
#' ### MLP
#' 
## ------------------------------------------------------------------------
set.seed(our_seed)
# 10-fold CV with 10 repetitions
trc <- trainControl (method="cv", number=10)
# we will train the model fixing a large number of units and using weight decay
(decays <- 10^seq(-2, 0, by=0.2))

# train the model
mlp.model <- train(form = type ~ .-quality,
               data = training.data,
               trControl = trc,
               method = "nnet",
               maxit = 200,
               tuneGrid = expand.grid(.size=20,.decay=decays),
               trace = FALSE)

# Table of results along with misclassification rate
(tab <- table(training.data$type, predict(mlp.model)))
(mlp.error <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
## ------------------------------------------------------------------------
mlp.model$finalModel

#' 
#' 
#' ### RBF
#' 
## ------------------------------------------------------------------------
rbf.model <- train(type~., 
    data = training.data, 
    method = "rbfDDA", 
    trControl = trainControl(method = "CV", number = 10))

(tab <- table(training.data$type, predict(rbf.model)))
(mlp.error <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

#' 
#' 
#' ### Random Forests
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
#' 
#' 
#' ## QUALITY PREDICTION
#' 
#' ### Multi-class Classification
#' 
## ------------------------------------------------------------------------
training.data$quality <- as.factor(training.data$quality)

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

(tab.w <- rda.kfcv(target = "quality", training.white, nfolds = 10))
(tab.r <- rda.kfcv(target = "quality", training.red, nfolds = 10))
(RDA.kfcv.err.w <- 1 - sum(tab.w[row(tab.w)==col(tab.w)])/sum(tab.w))
(RDA.kfcv.err.r <- 1 - sum(tab.r[row(tab.r)==col(tab.r)])/sum(tab.r))

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

(tab <- lda.kfcv(target = "quality", training.white[,1:12], nfolds = 10))
(LDA.kfcv.err.w <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))
(tab <- lda.kfcv(target = "quality", training.red[,1:12], nfolds = 10))
(LDA.kfcv.err.r <- 1 - sum(tab[row(tab)==col(tab)])/sum(tab))

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
plot(loocv.error, type="l", xaxt = "n", main = "kNN cv-error (original data)")
axis(1:sqrt(nrow(training.data)))
kmin.1 <- which.min(loocv.error[,2])
min1 <- loocv.error[kmin.1,]

# knn amb dades escalades
set.seed(our_seed)
loocv.error <- loop.k(scale(training.data[,1:11]), training.data$quality, sqrt(nrow(training.data)))
plot(loocv.error, type="l", xaxt = "n", main = "kNN cv-error (scaled data)")
axis(1:sqrt(nrow(training.data)))
kmin.2 <- which.min(loocv.error[,2])
min2 <- loocv.error[kmin.2,]

# knn amb dades LD-projectades
set.seed(our_seed)
mod.lda <- lda(quality~.-type, data = training.data)
training.lda <- as.matrix(training.data[,1:11]) %*% as.matrix(mod.lda$scaling)
loocv.error.lda <- loop.k(training.lda, training.data$quality, sqrt(nrow(training.data)))
plot(loocv.error.lda, type='l', xaxt='n', main = "kNN cv-error (LDA-projected data)")
axis(1:sqrt(nrow(training.data)))
kmin.3 <- which.min(loocv.error.lda[,2])
min3 <- loocv.error.lda[kmin.3,]

(list("minimum with original data" = min1, "minimum with scaled data" = min2, "minimum with LDA-projected data" = min3))

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
    adj.data <- dd[-fold,1:11]
    val <- dd[fold,1:11]
    predicted.class <- knn(train = scale(adj.data), test = scale(val), cl = dd[-fold,target], k = num.neighbors)
    t <- c(t, dd[fold,target])
    p <- c(p, predicted.class)
  }
  tab <- table(Truth = t, Pred = p)
}

#' 
#' 
#' 
## ------------------------------------------------------------------------
kfcv.err <- NULL
for (i in 1:59) {
  tab <- knn.kfcv(target = "quality", dd = training.data[,-13], num.neighbors = i)
  kNN.kfcv.err <- 1 - sum(tab[row(tab) == col(tab)])/sum(tab)
  kfcv.err <- c(kfcv.err, kNN.kfcv.err)
}
plot(c(1:59), kfcv.err, xlab = "number of neighbors", main = "10fold cv error for wine quality prediction")
(k.min <- which.min(kfcv.err))
kfcv.err[k.min]

#' 
#' 
#' #### Ordinal Regression
## ------------------------------------------------------------------------
ordreg.model <- train(quality~., 
    data = training.data, 
    method = "polr", 
    trControl = trainControl(method = "CV", number = 10))

(tab <- table(truth=training.data$quality, pred=predict(ordreg.model)))
(ordreg.err <- 1 - sum(tab[col(tab)==row(tab)])/sum(tab))

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
lambdas <- seq(6.5,7,0.01)

model.ridge <- lm.ridge(quality~., data = training.data, lambda = lambdas)
plot(lambdas, model.ridge$GCV, main="GCV of Ridge Regression", type="l", 
     xlab=expression(lambda), ylab="GCV")

# The optimal lambda is given by
(lambda.ridge <- lambdas[which.min(model.ridge$GCV)])
abline (v=lambda.ridge,lty=2)

#' 
## ------------------------------------------------------------------------
training.data$type <- as.factor(training.data$type)
ridge.mod <- lm.ridge(quality~., data = training.data, lambda = lambda.ridge)
(beta.ridge <- coef(ridge.mod))
temp.ridge.pred <- beta.ridge[1] + as.matrix(training.data[,1:11]) %*% beta.ridge[2:12]
ridge.pred <- round(temp.ridge.pred)



ridge.kfcv <- function(target, dd, nfolds = 10, seed = 46290) {
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
    ridge.mod <- lm.ridge(fmla, data = adj.data, lambda = lambda.ridge)
    beta.ridge <- coef(ridge.mod)
    temp.ridge.pred <- beta.ridge[1] + as.matrix(val) %*% beta.ridge[2:12]
    ridge.pred <- round(temp.ridge.pred)
    t <- c(t, val[,target])
    p <- c(p, ridge.pred)
  }
  mse <- mean((t-p)^2)
}

(ridge.kfcv.err <- ridge.kfcv("quality", training.data))


#' 
#' 
#' ##### LASSO (L^1-regularized)
#' 
## ------------------------------------------------------------------------
library(Matrix)
library(glmnet)

t <- as.matrix(training.data[,1:11], ncol = 11)

training.w <- t[training.data$type == "w",]
training.r <- t[training.data$type == "r",]

model.lasso.red <- cv.glmnet(as.matrix(training.w), as.numeric(training.data[training.data$type == "w",]$quality), nfolds = 10)
model.lasso.white <- cv.glmnet(as.matrix(training.r), as.numeric(training.data[training.data$type == "r",]$quality), nfolds = 10)

(lambda.lasso.w <- model.lasso.white$lambda.min)
(lambda.lasso.r <- model.lasso.red$lambda.min)

plot(model.lasso.red)
red.kfcv.err <- model.lasso.red$cvm[model.lasso.red$lambda == lambda.lasso.r]
white.kfcv.err <- model.lasso.red$cvm[model.lasso.red$lambda == lambda.lasso.r]

n.red <- nrow(training.r)
n.white <- nrow(training.w)

(LASSO.kfcv.err <- (n.red * red.kfcv.err + n.white * white.kfcv.err)/( n.red + n.white ) ) # error total, diria que la fórmula és correcta.

#' 
