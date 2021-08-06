library(quantreg)
library(rugarch)

calculate_bic <- function(ts, max_lag, quantile_list) {
  #' Calculates Bayesian Information Criterion (BIC)
  #'
  #' @param ts Time-series or vector
  #' @param max_lag The maximum number of lags
  #' @quantile_list The list of quantiles
  #'
  #' @return Returns the matrix od BIC values
  #' for input lags and quantiles
  #'
  ar_train <- ar_matrix(ts, max_lag)
  bic_matrix <- matrix(0, max_lag, length(quantile_list))
  for (lag in 1:max_lag) {
    for (n_quantile in 1:length(quantile_list)) {
      quantile <- quantile_list[n_quantile]
      qr <-
        rq(paste("target ~ ", paste(
          paste("lag", seq(1:lag), sep = "_"),
          collapse = "+"
        ), sep = ""),
        data = ar_train,
        tau = quantile
        )
      bic_matrix[lag, n_quantile] <-
        logLik.rq(qr)[1] * (-2) + lag * log(dim(ar_train)[1])
    }
  }
  return(bic_matrix)
}

qar <- function(ts_train, ts_test, lag, quantile) {
  #' Calculates predictions of quantile autoregression
  #'
  #' @param ts_train The training time-series
  #' @param ts_test The test time-series
  #' @param lag The number of lags
  #' @param quantile The quantile
  #'
  #' @return Returns the predictions of ts_test
  #' with the number of observations equal to
  #' length(ts_test) - lag
  #'
  ar_train <- ar_matrix(ts_train, lag)
  ar_test <- ar_matrix(ts_test, lag)
  qr <-
    rq(paste("target ~ ", paste(paste(
      "lag", seq(1:lag),
      sep = "_"
    ), collapse = "+"), sep = ""),
    data = ar_train,
    tau = quantile
    )
  target_hat <- predict.rq(qr, ar_test)
  return(target_hat)
}

backtesting <- function(quantile, outcomes, predictions) {
  #' Backtesting: the Kupiec unconditional and
  #' the Christoffersen conditional coverage tests
  #'
  #' @param quantile The quantile
  #' @param outcomes The actual outcomes
  #' @param predictions The predictions of outcomes
  #'
  #' @return The matrix of p-values and decisions
  #'
  columns_list <-
    c(
      "quantile",
      "expected.exceed",
      "actual.exceed",
      "uc.LRp",
      "cc.LRp",
      "uc.Decision",
      "cc.Decision"
    )
  vartest_list <- data.frame(matrix(
    0,
    1, length(columns_list)
  ))
  names(vartest_list) <- columns_list
  var_test <- VaRTest(quantile, outcomes, predictions)
  vartest_list[1, columns_list[1]] <- quantile
  for (i in 2:length(columns_list)) {
    vartest_list[[columns_list[i]]] <- var_test[[columns_list[i]]]
  }
  return(vartest_list)
}
