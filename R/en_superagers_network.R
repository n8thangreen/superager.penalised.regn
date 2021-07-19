
#' Elastic net superagers network
#'
#' Use (tidy) long data format as input.
#'
#' @param dat list of ct_long and sa_long dataframes
#' @param use caret package? FALSE is glmnet. They both use caret but have different arguments.
#' @param lambda oneSE or best?
#' @param lambda_grid Define lambdas in advance; vector
#' @param alpha real
#'
#' @export
#' @import caret glmnet
#' @return the result of running train() or cv.glmnet()
#' @export
#'
en_superagers_network <- function(dat,
                                  caret = TRUE,
                                  lambda = "oneSE",
                                  lambda_grid = NULL, # c(0.5, 0.4, 0.3, 0.2, 0.1, 0.01)
                                  alpha = 0.5) {

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

  # fit model --
  if (caret) {
    model <- train(
      status ~ .,
      data = dat,
      method = "glmnet",
      # tuneGrid = expand.grid(alpha = c(0, 0.5, 1),
      #                       lambda = lambda_grid,
      # preProcess = c("center", "scale"),
      trControl = trainControl(
        method = "cv",
        # method = "repeatedcv",
        # method = "LOOCV",  ##TODO: why doesn't this work?
        selectionFunction = lambda,
        number = 10#, # folds
        # repeats = 6
      ),
      tuneLength = 25    # number of alpha, lambda to try
    )
  } else {
    model <-
      # cv selected model
      cv.glmnet(x = as.matrix(dat %>% select(-status)),
                y =  as.factor(dat$status),
                family = "binomial",
                alpha = alpha,
                lambda = lambda_grid,
                standardize = TRUE,
                nfolds = 10)

      # user-defined model
      # glmnet(x = as.matrix(dat %>% select(-status)),
      #        y =  as.factor(dat$status),
      #        family = "binomial",
      #        #nlambda = 100,
      #        alpha = alpha,
      #        lambda = lambda_grid,
      #        standardize = TRUE,
      #        nfolds = 10)
  }

  model
}


