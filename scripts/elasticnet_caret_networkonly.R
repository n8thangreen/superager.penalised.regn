
# elastic net regression
# main script
# network only
# Nathan Green, UCL

# elastic net ref:
# www.sthda.com/english/articles/37-model-selection-essentials-in-r/
# 153-penalized-regression-essentials-ridge-lasso-elastic-net/#elastic-net

library(caret)
library(dplyr)
library(reshape2)
library(tidyr)
library(glue)
library(purrr)

# set.seed(5234)
set.seed(123)

## select tensor data

# TENSOR <- "3T"
TENSOR <- "7T"
# TENSOR <- "3T_xover"
# TENSOR <- "3T_quality"

# source("scripts/prep_data.R")

dat_list <- readRDS(file = glue("data/dat_list_{TENSOR}_n.RDS"))


############
# analysis #
############

load("data/regions.RData")
keep_network <- c("DMN", "Salience", "ECN_L", "ECN_R", "Hippocampal", "Language")

## fit model
res <-
  purrr::map(dat_list, ~en_superagers_network(.x))

saveRDS(res, file = glue("data/elasticnet_res_{TENSOR}.RDS"))
# res <- readRDS(file = glue("data/elasticnet_res_{TENSOR}.RDS"))

# print(res[[1]])
# plot(res[[1]])

# best tuning parameter
# alpha, lambda
bestTune <- purrr::map(res, ~.$bestTune)

finalModel <-
  purrr::map(res,
             function(x) coef(x$finalModel,
                              x$bestTune$lambda))

coef_ids <-
  purrr::map(finalModel, ~.@i) %>%
  setNames(keep_network)

coef_vals <- purrr::map(finalModel, ~.@x)

mcoed_ids <- melt(coef_ids) %>% set_names("Region", "network")
mcoed_vals <- melt(coef_vals)

# final output array
out <-
  cbind(mcoed_ids, mcoed_vals) %>%
  filter(Region != 0) %>%                # remove intercept
  mutate(or = round(exp(value), 4)) %>%  # convert to odds ratio
  merge(regions, by = "Region") %>%
  select(-L1) %>%
  arrange(network)

write.csv(out,
          file = glue("data/elastic_net_{TENSOR}_table.csv"))

or_table <- dcast(out,
                  network ~ Label,
                  value.var = "or")

or_table[is.na(or_table)] <- ""

write.csv(or_table,
          file = glue("data/elastic_net_{TENSOR}_table.csv"))


# plots and tables
source("scripts/output_plots.R")

