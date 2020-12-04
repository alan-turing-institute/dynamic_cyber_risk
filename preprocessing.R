# The file pre-process the data and creates "hacking_breaches.csv"

df <- read.csv("PRC Data Breach Chronology - 1.13.20.csv", header = T)
df <- df[df$Type.of.breach == "HACK", ]
df$total_records <- as.numeric(gsub(",", "", df$Total.Records, "[,]"))
df <- df[complete.cases(df$total_records) & (df$total_records > 0), ]
df$date <- as.Date(df$Date.Made.Public, "%m/%d/%Y")
df <- df[order(df$date), ]
df$date_next <- c(df$date[2:dim(df)[1]], NA)
df$time <- as.numeric(df$date_next - df$date)
set.seed(1)
for (i in 1:dim(df)[1]) {
  df$time[i] <- ifelse(df$time[i] == 0, runif(1, 0, 1), df$time[i])
}
df <- df[order(df$date, df$time), ]
df <- head(df, -1)
df$log_time <- log(df$time)
df$log_size <- log(df$total_records)
n_train <- floor(dim(df)[1] * 0.6)
df$flag_train <- 0
df$flag_train[1:n_train] <- 1
# write.csv(df, "hacking_breaches.csv", row.names = FALSE)
