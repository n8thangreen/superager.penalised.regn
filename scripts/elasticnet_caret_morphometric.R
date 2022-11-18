
# superagers analysis
# elastic net regression
# with thickness data


library(caret)
library(dplyr)
library(reshape2)
library(tidyr)
library(glue)
library(purrr)


# fit model
res <-
  purrr::map(dat_list_t1, ~en_superagers_network(.x))

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

# long format
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
          file = glue("data/elastic_net_t1_table.csv"))

or_table <- dcast(out,
                  network ~ Label,
                  value.var = "or")

or_table[is.na(or_table)] <- ""

write.csv(or_table,
          file = glue("data/elastic_net_t1_table.csv"))


# plots and tables
source("scripts/output_plots.R")


