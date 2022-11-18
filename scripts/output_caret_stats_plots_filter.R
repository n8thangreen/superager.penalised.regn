
# using the results of the elastic net
# model fits using caret package
# model performance statistics


library(caret)
library(dplyr)
library(knitr)
library(reshape2)
library(glue)


################
# read in data #
################

res_3T <- readRDS("data/final/elasticnet_res_3T.RDS")
res_7T <- readRDS("data/final/elasticnet_res_7T.RDS")

dat_list_3T <- readRDS("data/dat_list_3T_n.RDS")
dat_list_7T <- readRDS("data/dat_list_7T_n.RDS")

NETWORK_NAMES <- names(res_3T)
num_networks <- length(NETWORK_NAMES)

out_tab <- read.csv(file = glue("data/elasticnet_out_glmnet_7T.csv"))


##############
# calc stats #
##############

stat_res <- list()

for (i in seq_len(num_networks)) {

  tab <- filter(out_tab, network == NETWORK_NAMES[i])

  stat_res[["3T"]][[i]] <-
    fit_stats_filter(dat_list_3T[[i]],
                     res_3T[[i]],
                     single_coeff = FALSE,
                     tab)

  stat_res[["7T"]][[i]] <-
    fit_stats_filter(dat_list_7T[[i]],
                     res_7T[[i]],
                     single_coeff = FALSE,
                     tab)
}

names(stat_res[["3T"]]) <- NETWORK_NAMES
names(stat_res[["7T"]]) <- NETWORK_NAMES

# save(stat_res, file = "data/predict_output_filter.RData")


#########
# plots #
#########

## 7T

x11()
# tiff(filename = "output/grid_filter_7T.tiff", width = 800, height = 600)
par(mfrow = c(2,3), cex.axis = 2, mar = c(5.1, 5, 4.1, 5))
for (i in seq_len(num_networks)) {

  nm_regions <- names(stat_res$`7T`[[i]]$ppred)
  n_regions <- length(stat_res$`7T`[[i]]$ppred)

  plot(stat_res$`7T`[[i]]$obs_status,
       stat_res$`7T`[[i]]$ppred[[1]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`7T`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager",
       cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 2, cex = 2)

  legend("bottomright", lty = 1, col = 1:n_regions, nm_regions, bty = "n")

  for (j in 2:n_regions) {
    points(stat_res$`7T`[[i]]$obs_status,
           stat_res$`7T`[[i]]$ppred[[j]][, "1"],
           col = j)
    axis(1, at = c(1,2), labels = c("Control", "Superager"))
    abline(lm(stat_res$`7T`[[i]]$ppred[[j]][, "1"] ~ stat_res$`7T`[[i]]$obs_status), col = j)
  }
}
# dev.off()

## 3T

# x11()
# tiff(filename = "output/grid_filter_3T.tiff", width = 800, height = 600)
par(mfrow = c(2,3), cex.axis = 2, mar = c(5.1, 5, 4.1, 5))
for (i in seq_len(num_networks)) {

  nm_regions <- names(stat_res$`3T`[[i]]$ppred)
  n_regions <- length(stat_res$`3T`[[i]]$ppred)

  plot(stat_res$`3T`[[i]]$obs_status,
       stat_res$`3T`[[i]]$ppred[[1]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager",
       cex.lab = 2, cex.axis = 2, cex.main = 2, cex.sub = 2, cex = 2)

  legend("bottomright", lty = 1, col = 1:n_regions, nm_regions, bty = "n")

  for (j in 2:n_regions) {
    points(stat_res$`3T`[[i]]$obs_status,
           stat_res$`3T`[[i]]$ppred[[j]][, "1"],
           col = j)
    axis(1, at = c(1,2), labels = c("Control", "Superager"))
    abline(lm(stat_res$`3T`[[i]]$ppred[[j]][, "1"] ~ stat_res$`3T`[[i]]$obs_status), col = j)
  }
}
# dev.off()





####################
# ggplot version

# reshape to single long format
ppred <-
  stat_res$`7T` |>
  purrr::map("ppred") |>
  melt() |>
  rename(name = "L1",
         node = "L2",
         prob = "value")

obs_status <- stat_res$`7T`$DMN$obs_status

ggplot_dat <-
  as_tibble(ppred) |>
  filter(variable == 1) |>
  data.frame(obs_status = obs_status)

ggplot(ggplot_dat, aes(x = obs_status, y = prob, group = node, col = node)) +
  facet_wrap(~ name) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  ylim(0, 1) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1,2))


