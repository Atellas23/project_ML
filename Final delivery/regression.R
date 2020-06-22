### Regression for quality prediction

library(MASS)
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)
library(RSNNS)
library(doMC)
#registerDoMC(2)
ourseed <- 46290

## Data separation:

red_w <- read.table("winequality-red.csv", sep=';', header = TRUE)
white_w <- read.table("winequality-white.csv", sep=';', header = TRUE)
red_w <- distinct(red_w)
white_w <- distinct(white_w)

wine <- rbind(red_w, white_w)

N <- nrow(wine)                                                                                              
set.seed(our_seed)
sample_size <- floor(2/3 * nrow(wine))
training_indices <- sample(seq_len(nrow(wine)), size = sample_size)
training.data  <- wine[training_indices, ]
training.red <- training.data[training.data$type == "r",]
training.white <- training.data[training.data$type == "w",]
testing.data <- wine[-training_indices, ]
k <- 10

## LASSO

regression.model_lasso <- caret::train(quality ~ .,
              data = scale(training.data),
              trControl = trainControl(method = "cv", number = k),
              method = "lasso")

## Radial Basis Function

regression.model_RBF <- caret::train(quality ~ .,
             data = scale(training.data),
             trControl = trainControl(method = "cv", number = k),
             method = "rbfDAA")

## Random Forest

regression.model_RF <- caret::train(quality ~ .,
             data = scale(training.data),
             trControl = trainControl(method = "cv", number = k),
             method = "rf")

## Linear model

regression.model_lm <- caret::train(quality ~ .,
             data = scale(training.data),
             trControl = trainControl(method = "cv", number = k),
             method = "lm")

## MLP

regressio.model_mlp <- caret::train(quality ~ .,
             data = scale(training.data),
             trControl = trainControl(method = "cv", number = k),
             method = "mlpWeightDecay")

