# The file finds the regularization and standard deviation
#  parameters of CQAR on the training data

source("additional_functions.R")
source("main_functions.R")

df <- read.csv("hacking_breaches.csv", header = T)
ts_total <- df$log_time
n_train <- sum(df$flag_train)
ts_train <- ts_total[1:n_train]
vector_a <- c(0.1, 0.5, 1)
vector_sigma <- c(0.5, 0.7, 1)
accept_train <- matrix(0, length(vector_a), length(vector_sigma))
losses_train <- matrix(0, length(vector_a), length(vector_sigma))
for (i in seq_along(vector_a)) {
  for (j in seq_along(vector_sigma)) {
    reg_param <- vector_a[i]
    sigma_param <- vector_sigma[j]
    pred_train <- cqar(
      ts_train,
      lag = 5,
      quantile = 0.95,
      iter_number = 2500,
      burn_in_period = 500,
      reg_param,
      sigma_param,
      seed = 1
    )
    accept_train[i, j] <- mean(pred_train$accept_rate)
    losses_train[i, j] <- sum(pinball_loss(ts_train[6:n_train], pred_train$target_hat, quantile = 0.95))
  }
}

print(round(accept_train, 2))
print(round(losses_train, 2))