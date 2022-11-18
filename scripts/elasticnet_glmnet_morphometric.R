
# superagers analysis
# use glmnet directly, rather than caret
# so can limit max number of parameters in fitted model
# with morphometric data


library(caret)
library(dplyr)
library(reshape2)
library(tidyr)
library(glue)
library(purrr)


# source("scripts/prep_morphometric.R")

# dat_list_t1 <- readRDS(file = glue("data/dat_list_t1.RDS")) # complete data
dat_list_t1 <- readRDS(file = glue("data/dat_t1.RDS"))      # by network

# fit model
res <-
  purrr::map(dat_list_t1,
             function(.x) en_superagers_network(.x, caret = FALSE, pmax = 10))

map(res, ~.x[["lambda"]])

# hard-coded lambda = 0.1
## how does this affect the results?...
coeffs <- map(res, ~coef(.x, s = 0.2))

# final output array
out <-
  map(coeffs,
      function(.x)
        data.frame(value = .x@x,
                   ids = .x@i,
                   region = .x@Dimnames[[1]][.x@i + 1]) %>%
        mutate(or = round(exp(value), 4)))

out

out_tab <- bind_rows(out, .id = "network")
out_tab

# save
saveRDS(res, file = glue("data/elasticnet_res_glmnet_t1.RDS"))
write.csv(out_tab, file = glue("data/elasticnet_out_glmnet_t1.csv"))


