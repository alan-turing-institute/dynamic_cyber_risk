setwd("/Users/temp/Documents/QAR")
source("main_functions.R")
source("additional_functions.R")
library(dplyr)

df = read.csv("hacking_breaches.csv", header = T) 
ts_total = df$log_time
n_train = sum(df$flag_train)
n_test = length(ts_total) - n_train
ts_train = ts_total[1:n_train]
ts_test = ts_total[(n_train+1):length(ts_total)]

summary_stats = df%>%
  group_by(Type.of.organization) %>%
  summarise(min = min(time),
            median = median(time),
            mean = mean(time),
            sd = sd(time),
            max = max(time),
            total = n())

summary_stats[nrow(summary_stats)+1, ] = c("Total", df%>%
                       summarise(min = min(time),
                                 median = median(time),
                                 mean = mean(time),
                                 sd = sd(time),
                                 max = max(time),
                                 total = n()))

library(xtable)
xtable(summary_stats, ,digits=c(0, 0, 4, 2, 2, 2, 0, 0))

pdf(file = "plots/times.pdf")
par(mfrow=c(1, 1))
plot(as.Date(df$Date.Made.Public, "%m/%d/%Y"), df$time, type = "l", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "time", ylab = "", main = "inter-arrival times")
dev.off()

acf <- acf(ts_total)
pacf <- pacf(ts_total) 

pdf(file = "plots/acf_time.pdf")
par(mfrow=c(2, 1))
plot(acf, lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "lag", ylab = "ACF",  main = "log inter-arrival times")
plot(pacf, lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "lag", ylab = "PACF", main = "")
dev.off()

quantile_list = c(0.9, 0.92, 0.95)

bic = calculate_bic(ts_train, max_lag = 30, 0.5)
lag_optimal = apply(bic, 2, which.min)

pdf(file = "plots/bic_time.pdf")
par(mfrow=c(1, 1))
plot(bic, type = "l", lwd = 3, cex.main = 2, cex.lab = 2, cex.axis = 2, xlab = "lag", ylab = "", main = "BIC")
lines(which.min(bic), bic[which.min(bic)], type = "p", lwd = 12, col = "red")
dev.off()

qar_test = matrix(NaN, n_test, length(quantile_list))
waaqar_test = matrix(NaN, n_test, length(quantile_list))
losses_test = matrix(NaN, n_test, 2*length(quantile_list))
VaRtest_matrix = data.frame(matrix(0, 2*length(quantile_list), 7))
names(VaRtest_matrix) = c("quantile", "expected.exceed", "actual.exceed", "uc.LRp", "cc.LRp", "uc.Decision", "cc.Decision")
for (i in 1:length(quantile_list)) {
  lag = lag_optimal
  quantile = quantile_list[i]
  qar_test[(lag + 1):n_test, i] = qar(ts_train, ts_test, lag, quantile)
  waaqar_test[(lag + 1):n_test, i] = waaqar(ts_test, lag, quantile, iter_number = 2500, burn_in_period = 500, 
                                                     reg_param = 1, sigma_param = 0.7, seed = 1)$target_hat
  VaRtest_matrix[i, ] = backtesting(1-quantile, -ts_test[(lag + 1):n_test], -qar_test[(lag + 1):n_test, i])
  VaRtest_matrix[i+3, ] = backtesting(1-quantile, -ts_test[(lag + 1):n_test], -waaqar_test[(lag + 1):n_test, i])
  losses_test[(lag + 1):n_test, i] = pinball_loss(ts_test[(lag + 1):n_test], qar_test[(lag + 1):n_test, i], quantile)
  losses_test[(lag + 1):n_test, i+3] = pinball_loss(ts_test[(lag + 1):n_test], waaqar_test[(lag + 1):n_test, i], quantile)
}
VaRtest_matrix["quantile"] = rep(quantile_list, 2)
print(VaRtest_matrix)
xtable(VaRtest_matrix[1:3, ], digits=c(0, 2, 0, 0, 4, 4, 0, 0))
xtable(VaRtest_matrix[4:6, ], digits=c(0, 2, 0, 0, 4, 4, 0, 0))

cumlosses_test = apply(na.omit(losses_test), 2, cumsum)

regret_average = matrix(0, dim(cumlosses_test)[1], length(quantile_list))
for (i in 1:length(quantile_list)) {
  regret_average[, i] = (cumlosses_test[, i] - cumlosses_test[,i+3 ]) / seq(1, dim(cumlosses_test)[1])
}

pdf(file = "plots/qar_time.pdf")
range = seq((max(lag_optimal) + 1), n_test)
par(mfrow = c(1, 1))
plot(ts_test[range], type = "l", col = "black", xlab = "time", ylab = "log inter-arrival times", main = "QAR", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(qar_test[range, 1], col = "blue",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(qar_test[range, 2], col = "green",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(qar_test[range, 3], col = "red",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
legend("bottomleft",legend = c("actual", "0.90", "0.92", "0.95"), col=c("black", "blue", "green", "red"),lty=1.4, cex=1.3,  lwd = 1)
dev.off()

pdf(file = "plots/waaqar_time.pdf")
range = seq((max(lag_optimal) + 1), n_test)
par(mfrow = c(1, 1))
plot(ts_test[range], type = "l", col = "black", xlab = "time", ylab = "log inter-arrival times", main = "WAAQAR", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(waaqar_test[range, 1], col = "blue",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(waaqar_test[range, 2], col = "green",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(waaqar_test[range, 3], col = "red",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
legend("bottomleft", legend = c("actual", "0.90", "0.92", "0.95"), col=c("black", "blue", "green", "red"),lty=1.4, cex=1.3,  lwd = 1)
dev.off()

pdf(file = "plots/average_regret.pdf")
par(mfrow = c(1, 1))
plot(regret_average[, 1], type = "l",  col = "blue", xlab = "time", ylab = "average regret", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(regret_average[, 2], col = "green", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(regret_average[, 3], col = "red", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
legend("topright",legend = c("0.90", "0.92", "0.95"), col=c("blue", "green", "red"),lty=1, cex=1.3,  lwd = 1.4)
#lines(rep(0, length(Loss_waaqr90_time)))
dev.off()

