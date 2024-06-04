#EX D1
data <- read_csv("/DESKTOP/Statistix/TEMA D/probabilitati.csv")
head(data)
scores <- data$score
sample_mean <- mean(scores)

variance <- 69.420
std_dev <- sqrt(variance) 

n <- length(scores)

std_error <- std_dev / sqrt(n)

conf_level_95 <- 0.95
z_95 <- qnorm(1 - (1 - conf_level_95) / 2)
conf_interval_95 <- c(sample_mean - z_95 * std_error, sample_mean + z_95 * std_error)

conf_level_99 <- 0.99
z_99 <- qnorm(1 - (1 - conf_level_99) / 2)
conf_interval_99 <- c(sample_mean - z_99 * std_error,  sample_mean + z_99 * std_error)


cat("95% Intervalul de incredere e: [", conf_interval_95[1], ", ", conf_interval_95[2], "]\n")
cat("99% Intervalul de incredere e: [", conf_interval_99[1], ", ", conf_interval_99[2], "]\n")



#EX D2
data <- read.csv("statistica.csv")

sample <- data$punctaje 

sample_mean <- mean(sample)
sample_sd <- sd(sample)
sample_size <- length(sample)

confidence_interval <- function(mean, sd, n, confidence_level) 
{
  error_margin <- qnorm((1 + confidence_level) / 2) * (sd / sqrt(n))
  lower_bound <- mean - error_margin
  upper_bound <- mean + error_margin
  c(lower_bound, upper_bound)
}

conf_int_95 <- confidence_interval(sample_mean, sample_sd, sample_size, 0.95)
conf_int_99 <- confidence_interval(sample_mean, sample_sd, sample_size, 0.99)

cat("Intervalul de incredere de 95% e: [", conf_int_95[1], ", ", conf_int_95[2], "]\n")
cat("Intervalul de incredere de 99% e: [", conf_int_99[1], ", ", conf_int_99[2], "]\n")



#EX D3
sample_size <- 100
num_failures <- 14
p_hat <- num_failures / sample_size

p0 <- 0.15 #probabilitatea (sub ipoteza nula) ca schimbarea nu are loc

# Calcularea valorii z
z <- (p_hat - p0) / sqrt((p0 * (1 - p0)) / sample_size)

p_value <- 2 * (1 - pnorm(abs(z)))  

alpha_1 <- 0.01
alpha_5 <- 0.05

cat("Proportia observata e:", p_hat, "\n")
cat("Valoarea Z:", z, "\n")
cat("Valoarea p:", p_value, "\n")

if (p_value < alpha_1) 
{
  cat("La nivelul de semnificatie de 1%, constatam ca schimbarea a fost utila.\n")
} 
else 
{
  cat("La nivelul de semnificatie de 1%, constatam ca schimbarea a fost inutila.\n")
}

if (p_value < alpha_5) 
{
  cat("La nivelul de semnificatie de 5%, constatam ca schimbarea a fost utila.\n")
} else 
{
  cat("La nivelul de semnificatie de 5%, constatam ca schimbarea a fost inutila.\n")
}
