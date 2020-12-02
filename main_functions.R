library(tsutils)
library(ramify)

pinball_loss <- function(target, predictions, quantile) {
  # function calculates pinball loss for vectors outcomes and predictions
  # for quantile q
  loss <- target - predictions
  loss <- ifelse(loss > 0, quantile * loss, (quantile - 1) * loss)
  return(loss)
}

ar_matrix = function(x, n_lags) {
  lag_vector = (0:-n_lags)
  matrix = data.frame(lagmatrix(rev(x), lag_vector))
  names(matrix) = c("target", paste("lag", seq(1:n_lags), sep = "_"))
  matrix = na.omit(matrix)
  return(matrix[nrow(matrix):1, ])
}

waaqr<- function(target, lag_matrix, quantile, iter_number, burn_in_period, 
                  reg_param, sigma_param, seed) {
  # Inputs: outcomes - target vector, X - matrix of features where rows
  # are observations and columns are features q - quantile M - maximum
  # number of iteration M0 - the length of burn-in period a -
  # regualrization parameter sigma - standard deviation
  
  # Outputs: gamma - calculated predictions for WAAQR, theta - sampling
  # parameters at each iteration, lik - log-likelihood of parameters at
  # each iteration
  
  set.seed(seed)
  
  lag_matrix <- cbind(rep(1, dim(lag_matrix)[1]), lag_matrix)
  time <- dim(lag_matrix)[1]
  lag_number <- dim(lag_matrix)[2]
  
  loglik_params <- function(reg_param, quantile, theta, ksi, outcomes) {
    # likelihood of parameters theta
    time <- length(outcomes)
    losses <- pinball_loss(outcomes, ksi, quantile)
    loglik_theta <- -reg_param * sum(abs(theta)) - 1/sqrt(time) * sum(losses)
    return(loglik_theta)
  }
  
  loglik_params0 <- function(reg_param, theta) {
    # likelihood of parameters theta at time t = 0
    loglik_theta <- -reg_param * sum(abs(theta))
    return(loglik_theta)
  }
  
  target_hat <- matrix(0, time, ncol = 1)
  theta <- array(0, dim = c(time, iter_number, lag_number))
  accept_matrix <- array(0, dim = c(time, iter_number, lag_number))
  for (t in 1:time) {
    # initial estimates of theta
    if (t > 1) {
      theta[t, 1, ] <- theta[t - 1, iter_number, ]
    }
    ksi_total <- 0
    for (m in 2:iter_number) {
      theta_old <- matrix(theta[t, m - 1, ], ncol = lag_number)
      theta_new <- theta_old + matrix(rnorm(lag_number, 0, sigma_param^2), 
                                      nrow = 1, ncol = lag_number)  
      if (t > 1) {
        ksi_old <- lag_matrix[1:(t - 1), ] %*% t(theta_old)
        ksi_new <- lag_matrix[1:(t - 1), ] %*% t(theta_new)
        loglik_old <- loglik_params(reg_param, quantile, theta_old, 
                                    ksi_old, target[1:(t - 1)])
        loglik_new <- loglik_params(reg_param, quantile, theta_new, 
                                    ksi_new, target[1:(t - 1)])
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
    target_hat[t, ] <- ksi_total/(iter_number - burn_in_period)
  }
  return(list(target_hat = target_hat, theta = theta, accept_rate = mean(mean(accept_matrix))))
}

waaqar = function(ts, lag, quantile, iter_number, burn_in_period, 
          reg_param, sigma_param, seed) {
  ar_matrix = ar_matrix(ts, lag)
  target = as.vector(ar_matrix$target)
  lag_matrix = as.matrix(ar_matrix[, !names(ar_matrix) %in% c("target")])
  waaqr_list = waaqr(target, lag_matrix, quantile, iter_number, burn_in_period, 
           reg_param, sigma_param, seed)
  return(list(target_hat = waaqr_list$target_hat, theta = waaqr_list$theta, accept_rate = waaqr_list$accept_rate))
}
