set.seed(123)
x <- rexp(100, rate = 0.2)
x_log <- log(x)

par(mfrow = c(1, 2))
hist(x, breaks = 15, col = "skyblue", main = "Original", xlab = "x")
hist(x_log, breaks = 15, col = "tomato", main = "Log(x)", xlab = "log(x)")
par(mfrow = c(1, 1))

shapiro.test(x)
shapiro.test(x_log)

x_log_z <- scale(x_log)
x_log_mm <- (x_log - min(x_log)) / (max(x_log) - min(x_log))

data.frame(
  Original = c(media = mean(x), var = var(x), min = min(x), max = max(x)),
  Logaritmica = c(media = mean(x_log), var = var(x_log), min = min(x_log), max = max(x_log)),
  Log_Zscore = c(media = mean(x_log_z), var = var(x_log_z), min = min(x_log_z), max = max(x_log_z)),
  Log_MinMax = c(media = mean(x_log_mm), var = var(x_log_mm), min = min(x_log_mm), max = max(x_log_mm))
)

