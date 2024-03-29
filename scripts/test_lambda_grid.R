# elastic net regression
# how does lambda change between fits?
# N Green

# *notation*
#
# alpha: split between lasso and ridge regression
# lambda: penalty
#
# lambda*[(1 - alpha)*beta2 + alpha*beta1]


library(caret)
library(dplyr)
library(reshape2)
library(tidyr)
library(glue)
library(purrr)
library(glmnet)


# set.seed(5234)
set.seed(123)


#########################
# select tensor data

TENSOR <- "3T"
# TENSOR <- "7T"
# TENSOR <- "3T_xover"
# TENSOR <- "3T_quality"

# source("scripts/prep_data.R")
dat_list <- readRDS(file = glue("data/dat_list_{TENSOR}_n.RDS"))
# dat_list_t1 <- readRDS(file = glue("data/dat_list_t1.RDS"))


############
# analysis #
############
# varying alpha, lambda

keep_network <- c("DMN", "Salience", "ECN_L", "ECN_R", "Hippocampal", "Language")
alpha_seq <- c(0, 0.01, 0.1, 0.15, 0.5, 1)
lambda_seq <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.01)
res <- list()

for (j in keep_network) {

  # elastic net superagers by network
  res[[j]] <-
    en_superagers_grid(
      dat = dat_list[[j]],
      lambda_seq = lambda_seq,
      alpha_seq = alpha_seq)
}


##########
# plots

library(gridExtra)

ggres <- list()

for (i in names(res)) {
  ggres[[i]] <-
    ggplot(res[[i]], nameInStrip = TRUE, metric = "Accuracy") +
    ggtitle(i)
}

do.call("grid.arrange", c(ggres, ncol=2))
