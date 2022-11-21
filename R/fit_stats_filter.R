
#' Model fit statistics
#'
#' @param dat Data list of control and superager in long format.
#' @param res Results of model fit
#' @param tab Table of fitter coefficient values by region
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
fit_stats_filter <- function(dat, res, tab,
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

  obs_status <- as.numeric(dat$status)

  dat <- dat[, names(dat) != "status"]
  n_nodes <- ncol(dat)

  pred <- list()
  ppred <- list()

  nm <- names(dat)

  for (i in 1:nrow(tab)) {

    rgn <- tab$region[i]

    if (rgn == "(Intercept)") next

    Xmat <- dat

    if (single_coeff) {
      Xmat[, nm != rgn] <- 0
    } else {
      Xmat[[rgn]] <- 0
    }

    if (glmnet) {
      pred[[rgn]] <-
        predict(res,
                newx = as.matrix(Xmat),
                s = s,
                type = "class")

      ppred[[rgn]] <-
        predict(res,
                newx = as.matrix(Xmat),
                s = s,
                type = "response") # probability
    } else {
      pred[[rgn]] <- predict(res, Xmat, type = "raw")
      ppred[[rgn]] <- predict(res, Xmat, type = "prob")
    }
  }

  list(pred = pred,
       ppred = ppred,
       obs_status = obs_status)
}

