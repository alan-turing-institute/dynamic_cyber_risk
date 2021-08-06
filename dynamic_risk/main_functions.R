#' The file contains the implementations of
#'  Weak Aggregating Algorithm for Quantile Regression (WAAQR)
#'  and Competitive Quantile Autoregression (CQAR).

source("support_functions.R")

waaqr <-
  function(target,
           lag_matrix,
           quantile,
           iter_number,
           burn_in_period,
           reg_param,
           sigma_param,
           seed = NULL,
           theta_param = NULL) {
    #' Weak Aggregating Algorithm for Quantile Regression
    #'
    #' @description
    #' The function calculates the predictions of
    #' the quantile of target in online mode.
    #' At time step t the function predicts target[t]
    #' based on the signal lag_matrix[t],
    #' which contains the lagged values of target
    #'
    #' @param target The outcomes to predict, dim [number of steps, 1].
    #' @param lag_matrix The matrix of lagged values of target,
    #'   dim [number of steps, number of lags].
    #' @param quantile The predicted quantile, numeric.
    #' @param iter_number The total number of Markov Chain
    #'   Monte Carlo (MCMC) iterations, numeric.
    #' @param burn_in_period The burn-in period of MCMC, numeric
    #' @param reg_param The regulazation parameter, numeric.
    #' @param sigma_param The standard deviation parameter, numeric.
    #' @param seed The random seed, numeric.
    #' @param theta_param The initial parameters of quantile regression 
    #'     (if NULL sampling starts from zero).
    #'
    #' @return target_hat The predictions of outcomes, dim [number_of_steps, 1].
    #' @return theta The MCMC sampling parameters at each iteration,
    #'   dim [number_of steps, number of MCMC iterations, number of lags + 1].
    #' @return accept_rate The acceptance rate of the parameters theta, numeric.

    if (!is.null(seed)) {
      set.seed(seed)
    }
    lag_matrix <- cbind(rep(1, dim(lag_matrix)[1]), lag_matrix)
    time <- dim(lag_matrix)[1]
    lag_number <- dim(lag_matrix)[2]
    target_hat <- matrix(0, time, ncol = 1)
    theta <- array(0, dim = c(time, iter_number, lag_number))
    if (!is.null(theta_param)) {
      theta[1, 1, ] <- theta_param
    }
    accept_matrix <-
      array(0, dim = c(time, iter_number, lag_number))
    for (t in 1:time) {
      if (t > 1) {
        theta[t, 1, ] <- theta[t - 1, iter_number, ]
      }
      ksi_total <- 0
      for (m in 2:iter_number) {
        theta_old <- matrix(theta[t, m - 1, ], ncol = lag_number)
        theta_new <-
          theta_old + matrix(rnorm(lag_number, 0, sigma_param^2),
            nrow = 1,
            ncol = lag_number
          )
        if (t > 1) {
          ksi_old <- lag_matrix[1:(t - 1), ] %*% t(theta_old)
          ksi_new <- lag_matrix[1:(t - 1), ] %*% t(theta_new)
          loglik_old <- loglik_params(
            reg_param, quantile, theta_old,
            ksi_old, target[1:(t - 1)]
          )
          loglik_new <- loglik_params(
            reg_param, quantile, theta_new,
            ksi_new, target[1:(t - 1)]
          )
        } else {
          loglik_old <- loglik_params0(reg_param, theta_old)
          loglik_new <- loglik_params0(reg_param, theta_new)
        }
        alpha <- min(1, exp(loglik_new - loglik_old))
        if (alpha >= runif(1, 0, 1)) {
          theta[t, m, ] <- theta_new
          accept_matrix[t, m, ] <- 1
        } else {
          theta[t, m, ] <- theta[t, m - 1, ]
        }
        if (m > burn_in_period) {
          ksi <- lag_matrix[t, ] %*% theta[t, m, ]
          ksi_total <- ksi_total + ksi
        }
      }
      target_hat[t, ] <- ksi_total / (iter_number - burn_in_period)
    }
    return(list(
      target_hat = target_hat,
      theta = theta,
      accept_rate = mean(mean(accept_matrix))
    ))
  }

cqar <- function(ts,
                 lag,
                 quantile,
                 iter_number,
                 burn_in_period,
                 reg_param,
                 sigma_param,
                 seed = NULL,
                 theta_param = NULL) {
  #' Competitive Quantile Autoregression
  #'
  #' @description
  #' The function calculates the predictions
  #' of time-series ts in online mode.
  #' At time step t the function predicts
  #' the quantile of ts[t] based on the previous
  #' lagged values ts[t-1], ts[t-2],...,ts[t-lag].
  #' The first observation to predict is ts[lag+1].
  #'
  #' @param ts The time-series.
  #' @param lag The lag of ts, numeric.
  #' @param quantile The predicted quantile, numeric.
  #' @param iter_number The total number of Markov Chain
  #'   Monte Carlo (MCMC) iterations, numeric.
  #' @param burn_in_period The burn-in period of MCMC, numeric
  #' @param reg_param The regulazation parameter, numeric.
  #' @param sigma_param The standard deviation parameter, numeric.
  #' @param seed The random seed, numeric.
  #' @param theta_param The initial parameters of quantile autoregression 
  #'  (if NULL sampling starts from zero).
  #'
  #' @return target_hat The predictions of outcomes,
  #'   the length of the predictions is length(ts)-lag.

  #' @return theta The MCMC sampling parameters at each iteration,
  #'   dim [number_of steps, number of MCMC iterations, number of lags + 1].
  #' @return accept_rate The acceptance rate of the parameters theta, numeric.

  ar_matrix <- ar_matrix(ts, lag)
  target <- as.vector(ar_matrix$target)
  lag_matrix <-
    as.matrix(ar_matrix[, !names(ar_matrix) %in% c("target")])
  waaqr_list <- waaqr(
    target,
    lag_matrix,
    quantile,
    iter_number,
    burn_in_period,
    reg_param,
    sigma_param,
    seed,
    theta_param
  )
  return(
    list(
      target_hat = waaqr_list$target_hat,
      theta = waaqr_list$theta,
      accept_rate = waaqr_list$accept_rate
    )
  )
}
