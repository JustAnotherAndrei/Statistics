#EX D1
data <- read_csv("/DESKTOP/TEMA D/probabilitati.csv")
head(data)
scores <- data$score
sample_mean <- mean(scores)

variance <- 92.16
std_dev <- sqrt(variance) 

n <- length(scores)

std_error <- std_dev / sqrt(n)

conf_level_95 <- 0.95
z_95 <- qnorm(1 - (1 - conf_level_95) / 2)
conf_interval_95 <- c(sample_mean - z_95 * std_error, sample_mean + z_95 * std_error)

conf_level_99 <- 0.99
z_99 <- qnorm(1 - (1 - conf_level_99) / 2)
conf_interval_99 <- c(sample_mean - z_99 * std_error,  sample_mean + z_99 * std_error)


cat("95% Confidence Interval is: [", conf_interval_95[1], ", ", conf_interval_95[2], "]\n")
cat("99% Confidence Interval is: [", conf_interval_99[1], ", ", conf_interval_99[2], "]\n")
