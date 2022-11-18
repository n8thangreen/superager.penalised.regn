
# using the results of the elastic net
# model fits using caret package
# model performance statistics


library(caret)
library(dplyr)
library(knitr)
library(reshape2)


################
# read in data #
################

res_3T <- readRDS("data/final/elasticnet_res_3T.RDS")
res_7T <- readRDS("data/final/elasticnet_res_7T.RDS")
res_3T_xover <- readRDS("data/elasticnet_res_3T_xover.RDS")
res_3T_quality <- readRDS("data/elasticnet_res_3T_quality.RDS")

dat_list_3T <- readRDS("data/dat_list_3T_n.RDS")
dat_list_7T <- readRDS("data/dat_list_7T_n.RDS")
dat_list_3T_xover <- readRDS("data/dat_list_3T_xover_n.RDS")
dat_list_3T_quality <- readRDS("data/dat_list_3T_quality_n.RDS")

NETWORK_NAMES <- names(res_3T)
num_networks <- length(NETWORK_NAMES)


##############
# calc stats #
##############

stat_res <- list()

for (i in seq_len(num_networks)) {

  stat_res[["3T"]][[i]] <-
    fit_stats(dat_list_3T[[i]],
              res_3T[[i]])

  stat_res[["3T"]][[i]] <-
    fit_stats_filter(dat_list_3T[[i]],
              res_3T[[i]])

  stat_res[["7T"]][[i]] <-
    fit_stats(dat_list_7T[[i]],
              res_7T[[i]])

  stat_res[["3T_xover"]][[i]] <-
    fit_stats(dat_list_3T_xover[[i]],
              res_3T_xover[[i]])

  stat_res[["3T_quality"]][[i]] <-
    fit_stats(dat_list_3T_quality[[i]],
              res_3T_quality[[i]])
}

names(stat_res[["3T"]]) <- NETWORK_NAMES
names(stat_res[["7T"]]) <- NETWORK_NAMES
names(stat_res[["3T_xover"]]) <- NETWORK_NAMES
names(stat_res[["3T_quality"]]) <- NETWORK_NAMES

save(stat_res, file = "data/predict_output.RData")


# # compare directly
# models <- list(res_3T = res_3T,
#                res_7T = res_7T)
#
# resamples(models) %>%
#   summary(metric = "RMSE")


#########
# plots #
#########

# x11()
png("output/rgn_scatterplot_3T.png")
par(mfrow = c(2,3))
for (i in 1:num_networks) {
  plot(stat_res$`3T`[[i]]$obs_status,
       stat_res$`3T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T`[[i]][[2]][, "1"] ~ stat_res$`3T`[[i]]$obs_status))
}
dev.off()

# x11()
png("output/rgn_scatterplot_7T.png")
par(mfrow = c(2,3))
for (i in 1:num_networks) {
  plot(stat_res$`7T`[[i]]$obs_status,
       stat_res$`7T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`7T`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`7T`[[i]][[2]][, "1"] ~ stat_res$`7T`[[i]]$obs_status))
}
dev.off()

# x11()
png("output/rgn_scatterplot_3T_xover.png")
par(mfrow = c(2,3))
for (i in 1:num_networks) {
  plot(stat_res$`3T_xover`[[i]]$obs_status,
       stat_res$`3T_xover`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T_xover`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T_xover`[[i]][[2]][, "1"] ~ stat_res$`3T_xover`[[i]]$obs_status))
}
dev.off()

# x11()
png("output/rgn_scatterplot_3T_quality.png")
par(mfrow = c(2,3))
for (i in 1:num_networks) {
  plot(stat_res$`3T_quality`[[i]]$obs_status,
       stat_res$`3T_quality`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = names(stat_res$`3T_quality`)[i],
       xlab = "Observed status",
       ylab = "Model probablity superager")
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T_quality`[[i]][[2]][, "1"] ~ stat_res$`3T_quality`[[i]]$obs_status))
}
dev.off()


###################################
# single plot for 3T and 7T

# png("output/rgn_scatterplot_3T_7T.png")
# tiff("output/rgn_scatterplot_3T_7T.tiff", res=300,  width = 8, height = 8, units = 'in')
pdf("output/rgn_scatterplot_3T_7T.pdf")
par(mfrow = c(2,3))
for (i in 1:num_networks) {
  plot(x = stat_res$`3T`[[i]]$obs_status + rnorm(n = 31, 0, 0.04),
       y = stat_res$`3T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       main = gsub("_", " ", names(stat_res$`3T`)[i]),
       xlab = "Observed status",
       ylab = "Model probablity superager",
       col = "blue",
       pch = 19)
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`3T`[[i]][[2]][, "1"] ~ stat_res$`3T`[[i]]$obs_status), col = "blue")

  points(stat_res$`7T`[[i]]$obs_status + rnorm(n = 21, 0, 0.04),
       stat_res$`7T`[[i]][[2]][, "1"],
       ylim = c(0,1),
       xaxt = "n",
       xlab = "Observed status",
       ylab = "Model probablity superager",
       col = "red",
       pch = 19)
  axis(1, at = c(1,2), labels = c("Control", "Superager"))
  abline(lm(stat_res$`7T`[[i]][[2]][, "1"] ~ stat_res$`7T`[[i]]$obs_status), col = "red")
}
# plot(1, type = "n", axes=FALSE, xlab="", ylab="")
# legend(x = "top", bty = "n",
#        legend = c("3T", "7T"),
#        col = c("blue", "red"), lwd=2, cex=1, horiz = FALSE)
dev.off()


## ggplot version

# reshape to single long format
ppred <- purrr::map(stat_res$`7T`, "ppred")
obs_status <- stat_res$`7T`$DMN$obs_status

ggplot_dat <-
  do.call(rbind, ppred) |>
  as_tibble(rownames = "name") |>
  tidyr::separate(name, c("node", "id"), extra = "drop",
                  sep = "\\.", fill = "right") |>
  cbind(obs_status) |>
  select(-"0") |>
  rename("prob" = "1")

ggplot(ggplot_dat, aes(x = obs_status, y = prob)) +
  facet_wrap(~ node) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ylim(0, 1) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1,2))




## binned residuals?
##TODO:


######################
# contingency tables

purrr::map(stat_res$`3T`, ~confusionMatrix(.x$pred,
                as.factor(.x$obs_status - 1),
                dnn = c("pred", "obs"))$table %>% kable())
purrr::map(stat_res$`7T`, ~confusionMatrix(.x$pred,
                as.factor(.x$obs_status - 1),
                dnn = c("pred", "obs"))$table %>% kable())


##TODO: combined test as AND or OR


