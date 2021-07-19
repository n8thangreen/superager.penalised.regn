
# superagers analysis
# elastic net regression
# with thickness data


library(caret)
library(dplyr)
library(reshape2)
library(tidyr)
library(glue)
library(purrr)


set.seed(123)

ctr_original <-
  data.frame(readRDS(
    here::here("../../raw_data/081120_Demetrius/ctr_t1.rds")))

sa_original <-
  data.frame(readRDS(
    here::here("../../raw_data/081120_Demetrius/sa_t1.rds")))

# convert to long format
# equivalent to MRI input data

sa_long <-
  sa_original %>%
  select(-Machine, -Type, -Subject, -Gender,
         -Age, -Manufacturer, -Model, -Date_GraphICA) %>%
  mutate(id = 1:n()) %>%
  melt(id.var = "id",
       variable.name = "region") %>%
  mutate(Label = region)

ct_long <-
  ctr_original %>%
  select(-Machine, -Type, -Subject, -Gender,
         -Age, -Manufacturer, -Model, -Date_GraphICA) %>%
  mutate(id = 1:n()) %>%
  melt(id.var = "id",
       variable.name = "region") %>%
  mutate(Label = region)

dat_list_t1 <- list(ct_long = ct_long,
                    sa_long = sa_long)

############
# analysis #
############

# fit model
res <- en_superagers_network(dat_list_t1)

# best tuning parameter
bestTune <- res$bestTune

finalModel <- coef(res$finalModel,
                   res$bestTune$lambda)

coef_ids <- finalModel@i
coef_vals <- finalModel@x
exp_coef_vals <- exp(coef_vals)

melt_id <- melt(coef_ids)
melt_or <- melt(exp_coef_vals)

or <-
  cbind(melt_id,
        or = melt_or[, 1]) %>%
  mutate(or = round(or, 4))


