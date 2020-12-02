setwd("/Users/temp/Documents/QAR")
source("main_functions.R")
source("additional_functions.R")
library(dplyr)

df = read.csv("hacking_breaches.csv", header = T) 
ts_total = df$log_size
n_train = sum(df$flag_train)
n_test = length(ts_total) - n_train
ts_train = ts_total[1:n_train]
ts_test = ts_total[(n_train+1):length(ts_total)]

summary_stats = df%>%
  group_by(Type.of.organization) %>%
  summarise(min = min(total_records),
            median = median(total_records),
            mean = mean(total_records),
            sd = sd(total_records),
            max = max(total_records),
            total = n())

summary_stats[nrow(summary_stats)+1, ] = c("Total", df%>%
  summarise(min = min(total_records),
            median = median(total_records),
            mean = mean(total_records),
            sd = sd(total_records),
            max = max(total_records),
            total = n()))

library(xtable)
xtable(summary_stats, ,digits=c(rep(0, ncol(summary_stats)+1)))

pdf(file = "plots/breaches.pdf")
par(mfrow=c(1, 1))
plot(as.Date(df$Date.Made.Public, "%m/%d/%Y"), df$total_records, type = "l", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "time", ylab = "", main = "breach sizes")
dev.off()

pdf(file = "plots/logbreaches.pdf")
par(mfrow=c(1, 1))
plot(as.Date(df$Date.Made.Public, "%m/%d/%Y"), df$log_size, type = "l", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "time", ylab = "", main = "log breach sizes")
dev.off()

acf <- acf(ts_total)
pacf <- pacf(ts_total) 

pdf(file = "plots/acf_size.pdf")
par(mfrow=c(2, 1))
plot(acf, lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "lag", ylab = "ACF",  main = "log breach sizes")
plot(pacf, lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3, xlab = "lag", ylab = "PACF", main = "")
dev.off()

quantile_list = c(0.9, 0.92, 0.95)

bic = calculate_bic(ts_train, max_lag = 30, 0.5)
lag_optimal = apply(bic, 2, which.min)

pdf(file = "plots/bic_size.pdf")
par(mfrow=c(1, 1))
plot(bic, type = "l", lwd = 3, cex.main = 2, cex.lab = 2, cex.axis = 2, xlab = "lag", ylab = "", main = "BIC")
lines(which.min(bic), bic[which.min(bic)], type = "p", lwd = 12, col = "red")
dev.off()

qar_test = matrix(NaN, n_test, length(quantile_list))
VaRtest_matrix = data.frame(matrix(0, length(quantile_list), 7))
names(VaRtest_matrix) = c("quantile", "expected.exceed", "actual.exceed", "uc.LRp", "cc.LRp", "uc.Decision", "cc.Decision")
for (i in 1:length(quantile_list)) {
  quantile = quantile_list[i]
  qar_test[(lag_optimal + 1):n_test, i] = qar(ts_train, ts_test, lag_optimal, quantile)
  VaRtest_matrix[i, ] = backtesting(1-quantile, -ts_test[(lag_optimal + 1):n_test], -qar_test[(lag_optimal + 1):n_test, i])
}
VaRtest_matrix["quantile"] = quantile_list
print(VaRtest_matrix)
xtable(VaRtest_matrix, digits=c(0, 2, 0, 0, 4, 4, 0, 0))

pdf(file = "plots/qar_size.pdf")
range = seq((lag_optimal + 1), n_test)
par(mfrow = c(1, 1))
plot(ts_test[range], type = "l", col = "black", xlab = "time", ylab = "log breach sizes", main = "QAR", lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(qar_test[range, 1], col = "blue",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(qar_test[range, 2], col = "green",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
lines(qar_test[range, 3], col = "red",  lwd = 1.4, cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.3)
legend("bottomleft", legend = c("actual", "0.90", "0.92", "0.95"), col=c("black", "blue", "green", "red"),lty=1.4, cex=1.3,  lwd = 1)
dev.off()
