
# RMSE, R-squared
# obs vs predicted
#
fit_stats <- function(dat, res) {

  ct_long <- dat$ct_long
  sa_long <- dat$sa_long

  # transform to wide format
  controls <-
    ct_long %>%
    arrange(region) %>%
    dcast(id ~ Label, value.var = "value") %>%
    mutate(status = 0) %>%
    select(-id)

  superagers <-
    sa_long %>%
    arrange(region) %>%
    dcast(id ~ Label, value.var = "value") %>%
    mutate(status = 1) %>%
    select(-id)

  dat <-
    rbind(controls, superagers) %>%
    as.data.frame() %>%
    mutate(status = as.factor(status))

  pred <- predict(res, dat, type = "raw")
  ppred <- predict(res, dat, type = "prob")

  # pred.glmnet <-
  #   predict(res$finalModel,
  #           newx = dat,
  #           type = "response",
  #           s = res$bestTune$lambda)
  #           # s = "lambda.min")

  # prediction performance against data
  stats <-
    data.frame(
      RMSE = RMSE(as.numeric(pred),
                  as.numeric(dat$status)),
      Rsquare = R2(as.numeric(pred),
                   as.numeric(dat$status)))

  list(pred = pred,
       # pred.glmnet = pred.glmnet,
       ppred = ppred,
       stats = stats,
       obs_status = as.numeric(dat$status))
}

