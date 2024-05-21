

# Functia pentru simularea unei variabile aleatoare finite
simulare_variabila_aleatoare <- function(valori, probabilitati, n = 1) {
  if (length(valori) != length(probabilitati)) {
    stop("Dimensiunile vectorilor de valori si probabilitati trebuie sa fie egale!")
  }
  if (sum(probabilitati) != 1) {
    stop("Probabilitatile trebuie sa insumeze 1!")
  }
  
  sample(valori, size = n, replace = TRUE, prob = probabilitati)
}

# Exemplu
valori <- c("x1", "x2", "x3", "x4")
probabilitati <- c(0.1, 0.4, 0.2, 0.3)

set.seed(123) # pentru reproductibilitate
rezultat <- simulare_variabila_aleatoare(valori, probabilitati, n = 10)
print(rezultat)
