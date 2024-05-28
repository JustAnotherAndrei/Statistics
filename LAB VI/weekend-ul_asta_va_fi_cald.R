#EX II.6

zconfidence_interval <- function(n, xn, sigma, alpha) 
{
  z <- qnorm(1 - alpha / 2)
  margine_eroare <- z * (sigma / sqrt(n))
  margine_sup <- xn - margine_eroare
  margine_inf <- xn + margine_eroare
  return(c(margine_inf,margine_sup))
}

zconfidence_interval_from_file <- function(file_path, sigma, alpha) 
{
  data <- read.table(file_path, header = FALSE)
  sample <- data$V1
  n <- length(sample)
  xn <- mean(sample)
  return(zconfidence_interval(n, xn, sigma, alpha))
}

file_path <- "history.txt"
sigma <- 5
alpha <- 0.05

confidence_interval <- zconfidence_interval_from_file(file_path, sigma, alpha)
print(confidence_interval)




#EX III.4

t_conf_interval <- function(n, xn, s, alpha)
{
  t <- qt(1 - alpha / 2, df = n - 1)
  margine_eroare <- t * (s / sqrt(n))
  margine_inf <- xn - margine_eroare
  margine_sup <- xn + margine_eroare
  return(c(lower_bound, upper_bound))
}

t_conf_interval_from_file <- function(file_path, alpha)
{
  data <- read.table(file_path, header = FALSE)
  sample <- data$V1
  n <- length(sample)
  xn <- mean(sample)
  s <- sd(sample)
  return(t_conf_interval(n, xn, s, alpha))
}

file_path <- "history.txt"
alpha_95 <- 0.05
alpha_99 <- 0.01

confidence_interval_95 <- t_conf_interval_from_file(file_path, alpha_95)
confidence_interval_99 <- t_conf_interval_from_file(file_path, alpha_99)

print(paste("Intervalul de incredere de 95% e:", confidence_interval_95))
print(paste("Interval de incredere de 99% e:", confidence_interval_99))




#EX IV.2

p0 <- 0.10
n <- 150
x <- 20
alpha <- 0.05

test_result <- prop.test(x, n, p = p0, alternative = "greater")

print(test_result)

if (test_result$p.value < alpha) 
{
  cat("Putem afirma cu nivel de semnificatie de 5% ca procentul componentelor defecte e mai mare decat 10%!\n")
} 

else
{
  cat("Nu putem afirma cu nivel de semnificatie de 5% ca procentul componentelor defecte e mai mare decat 10%!\n")
}
