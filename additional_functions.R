library(quantreg)
library(rugarch)

calculate_bic = function(target_train, max_lag, quantile_list) {
  ar_train = ar_matrix(target_train, max_lag)
  bic_matrix = matrix(0, max_lag, length(quantile_list))
  for (lag in 1:max_lag) {
    for (n_quantile in 1:length(quantile_list)) {
      quantile = quantile_list[n_quantile]
      qr <- rq(paste("target ~ ", paste(paste("lag", seq(1:lag), sep = "_"), collapse="+"), sep = ""), data=ar_train, tau = quantile)
      bic_matrix[lag, n_quantile] = logLik.rq(qr)[1] * (-2) + lag * log(dim(ar_train)[1]) 
    }
  }
  return(bic_matrix)
}

qar = function(ts_train, ts_test, lag, quantile) {
  ar_train = ar_matrix(ts_train, lag)
  ar_test = ar_matrix(ts_test, lag)
  qr <- rq(paste("target ~ ", paste(paste("lag", seq(1:lag), sep = "_"), collapse="+"), sep = ""), data=ar_train, tau = quantile)
  target_hat = predict.rq(qr, ar_test)
  return(target_hat)
}

backtesting = function(quantile, outcomes, predictions)  {
  columns_list = c("quantile", "expected.exceed", "actual.exceed", "uc.LRp", "cc.LRp", "uc.Decision", "cc.Decision")
  VaRtest_list = data.frame(matrix(0, 1, length(columns_list)))
  names(VaRtest_list) = columns_list
  VaR_test = VaRTest(quantile, outcomes, predictions)
  VaRtest_list[1, columns_list[1]] = quantile
  for (i in 2:length(columns_list)) {
    VaRtest_list[[columns_list[i]]] = VaR_test[[columns_list[i]]]
  }
  return(VaRtest_list)
}
