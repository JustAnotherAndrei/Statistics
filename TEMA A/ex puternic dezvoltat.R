# Fct pt calc probabilitatilor pt distributiile poission, geometrica si binomiala
calculateProbabillities <- function(lambda, p, n, m, k) {
  df <- data.frame(k = k:m,
                   Poisson = dpois(k:m, lambda),
                   Geometric = dgeom(k:m, p),
                   Binomial = dbinom(k:m, n, p)
                  )
  return(df)
}

# Fct pt reprezentarea grafica a functiilor de masa de probabilitate
plotProbabillities <- function(df) {
  df_long <- df %>%
    pivot_longer(-k, names_to = "Distribution", values_to = "Probabillity")
  
  ggplot(df_long, aes(x = k, y = Probabillity, color = Distribution)) +
    geom_line() +
    labs(x = "k", y = "Probabillity", title = "Probabillity Mass Functions")
}

# Apelul fct cu parametrii specificati
lambda <- 2
p <- 0.5
n <- 10
m <- 7
k <- 3

probabillities <- calculateProbabillities(lambda, p, n, m, k)
print(probabilities)

plotProbabillities(probabillities)

# Determinarea celei mai mici valori a lui k0 pentru care P(Y <= k0) > 1 - 10^-6 pentru distributia poisson
k0 <- qpois(1 - 1e-6, lambda)
print(k0)