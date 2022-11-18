
#' Model fit statistics
#'
#' @param dat Data list of control and superager in long format.
#' @param res
#' @param single_coeff Should we remove all but one of the coefficients?
#'    Alternative is to do the opposite and remove only one and leave the others.
#' @param glmnet Logical. Which package engine to use.
#' @param s lambda; default \code{NA}
#'
#' @return RMSE, R-squared, obs vs predicted
#' @references \url{https://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#'             \url{https://glmnet.stanford.edu/articles/glmnet.html}
#' @export
#'
fit_stats_filter <- function(dat,
                             res,
                             tab,
                             single_coeff = FALSE,
                             glmnet = FALSE,
                             s = NA) {

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

  pred <- list()
  ppred <- list()

  for (i in 1:nrow(tab)) {

    rgn <- tab$region[i]

    if (rgn == "(Intercept)") next

    dat_filter <- dat

    if (single_coeff) {
      nm <- names(dat_filter)
      dat_filter[, nm != rgn] <- 0
    } else {
      dat_filter[[rgn]] <- 0
    }

    if (glmnet) {
      pred[[rgn]] <-
        predict(res,
                newx = as.matrix(dat_filter[, 1:(ncol(dat_filter) - 1)]),
                s = s,
                type = "class")

      ppred[[rgn]] <-
        predict(res,
                newx = as.matrix(dat_filter[, 1:(ncol(dat_filter) - 1)]),
                s = s,
                type = "response") # probability
    } else {
      pred[[rgn]] <- predict(res, dat_filter, type = "raw")
      ppred[[rgn]] <- predict(res, dat_filter, type = "prob")
    }
  }

  list(pred = pred,
       ppred = ppred,
       obs_status = as.numeric(dat$status))
}

