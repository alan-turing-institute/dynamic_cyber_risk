#' Supporting functions for the main algorithms

library(tsutils)
library(ramify)

pinball_loss <- function(target, predictions, quantile) {
  #' The pinball loss between the targets and predictions
  #'
  #' @param target The outcomes, dim [number of steps, 1].
  #' @param predictions The predictions, dim [number of steps, 1].
  #' @param quantile The quantile of the pinball loss
  #'
  #' @return The pinball loss
  #'
  #' @example
  #' target = c(2, 3)
  #' predictions = c(1, 4)
  #' quantile = 0.1
  #' pinball_loss(target, predictions, quantile)
  #' 0.1 0.9
  #'
  loss <- target - predictions
  loss <- ifelse(loss > 0, quantile * loss, (quantile - 1) * loss)
  return(loss)
}

ar_matrix <- function(ts, n_lags) {
  #' Creates autoregressive matrix
  #'
  #' @param ts Time-series or vector
  #' @param n_lags The number of lags
  #'
  #' @return Returns the data frame with target
  #'   and lagged values
  #' @example
  #' ar_matrix(seq(1, 6), 3)
  #' target lag_1 lag_2 lag_3
  #'    4     3     2     1
  #'    5     4     3     2
  #'    6     5     4     3
  #'
  lag_vector <- (0:-n_lags)
  matrix <- data.frame(tsutils::lagmatrix(rev(ts), lag_vector))
  names(matrix) <-
    c("target", paste("lag", seq(1:n_lags), sep = "_"))
  matrix <- na.omit(matrix)
  return(matrix[nrow(matrix):1, ])
}

loglik_params <-
  function(reg_param,
           quantile,
           theta,
           ksi,
           outcomes) {
    #' Calculates the likelihood of parameters theta at time step > 1
    #'
    #' @param reg_param The regulazation parameter.
    #' @param quantile The predicted quantile.
    #' @param theta The MCMC sampling parameters.
    #' @param ksi The predictions of outcomes.
    #' @param outcomes The actual outcomes.
    #'
    #' @return Returns the likelihood of the sampling
    #'   parameters theta
    #'
    time <- length(outcomes)
    losses <- pinball_loss(outcomes, ksi, quantile)
    loglik_theta <-
      -reg_param * sum(abs(theta)) - 1 / sqrt(time) * sum(losses)
    return(loglik_theta)
  }

loglik_params0 <- function(reg_param, theta) {
  #' Calculates the likelihood of parameters theta at the first time step
  #'
  #' @param reg_param The regulazation parameter.
  #' @param theta The MCMC sampling parameters.
  #'
  #' @return Returns the likelihood of the sampling
  #'   parameters theta
  #'
  loglik_theta <- -reg_param * sum(abs(theta))
  return(loglik_theta)
}
