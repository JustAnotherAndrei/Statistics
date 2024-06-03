#EX A1


#a)
calculeaza_probabilitati <- function(lambda, p, n, m, k)
{
  if (k < 0 || k >= m || m > n)
  {
    stop("Parametrii trebuie sa satisfaca: 0 <= k < m <= n")
  }
  
  poisson_prob <- dpois(k:m, lambda)
  
  geometric_prob <- dgeom(k:m, p)
  
  binomial_prob <- dbinom(k:m, n, p)
  
  list(poisson = poisson_prob, geometric = geometric_prob, binomial = binomial_prob)
}

lambda <- 3.5
p <- 0.4
n <- 10
m <- 5
k <- 2

probabillities <- calculate_probabillities(lambda, p, n, m, k)
print(probabillities)


b)
  plot_probabillities <- function(lambda, p, n, m, k) 
{
  probs <- calculate_probabillities(lambda, p, n, m, k)
  
  k_vals <- k:m

  plot(k_vals, probs$poisson, type = "b", col = "blue", pch = 19, xlab = "k", ylab = "Probabilitate", main = "Functii de masa de probabilitate")
  lines(k_vals, probs$poisson, type = "b", col = "blue", pch = 19)
  
  points(k_vals, probs$geometric, type = "b", col = "red", pch = 19)
  lines(k_vals, probs$geometric, type = "b", col = "red", pch = 19)
  
  points(k_vals, probs$binomial, type = "b", col = "green", pch = 19)
  lines(k_vals, probs$binomial, type = "b", col = "green", pch = 19)
  
  legend("topright", legend = c("Poisson", "Geometric", "Binomial"), col = c("blue", "red", "green"), pch = 19)
}

plot_probabilities(lambda, p, n, m, k)


#c)
  find_k0 <- function(lambda)
{
  prag <- 1 - 10^(-6)
  prob_cumulata <- 0
  k0 <- 0
  
  while (cum_prob_cumulata <= threshold) 
  {
    prob_cumulata <- ppois(k0, lambda)
    k0 <- k0 + 1
  }
  
  return(k0 - 1)  
}

# Exemplu de utilizare
k0 <- find_k0(lambda)
print(k0)




#EX A2


#a)
read_and_analize_notes <- function(file_path) 
{
  data <- read.table(file_path, header = TRUE)
  
  sample_P <- data$P
  sample_S <- data$S
  
  freq_abs_P <- table(sample_P)
  freq_rel_P <- as.vector(freq_abs_P) / length(sample_P)
  
  freq_abs_S <- table(sample_S)
  freq_rel_S <- as.vector(freq_abs_S) / length(sample_S)
  
  mean_P <- mean(sample_P)
  mean_S <- mean(sample_S)
  
  list(freq_abs_P = freq_abs_P, freq_rel_P = freq_rel_P,   mean_P = mean_P, freq_abs_S = freq_abs_S, freq_rel_S = freq_rel_S, mean_S = mean_S)
}

results <- read_and_analize_notes("note PS.txt")
print(results)


#b)
remove_outliers_and_plot <- function(file_path, sample_name) 
{
  data <- read.table(file_path, header = TRUE)
  
  if (!(sample_name %in% names(data)))
  {
    stop("Numele esantionului nu e valid.")
  }
  
  sample <- data[[sample_name]]
  
  # determinam valorile inutile folosind metoda Interquartile Randge (IQR)
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # eliminam valorile inutile (cred)
  clean_sample <- sample[sample >= lower_bound & sample <= upper_bound]
  
  # reprezentam grafic distributia frecventelor
  breaks <- seq(1, 10, by = 1)
  hist(clean_sample, breaks = breaks, right = TRUE, col = "blue", main = paste("Distributia frecventelor pentru ", sample_name), xlab = "Note", ylab = "Frecvente")
  
  return(clean_sample)
}

clean_sample_P <- remove_outliers_and_plot("note PS.txt", "P")
clean_sample_S <- remove_outliers_and_plot("note PS.txt", "S")

