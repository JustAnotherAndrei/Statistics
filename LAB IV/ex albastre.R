
#========= EX I.2  =========



num_valori <- 10000

parabola <- function(x) -2 * x^2 + 5 * x - 2

x_valori <- runif(num_valori, min = 0, max = 2)

y_valori <- parabola(x_valori)

aria_aproximativa <- sum(y_valori) * 2 / num_valori

aria_exacta <- integrate(parabola, lower = 0, upper = 2)$value

eroare_relativa <- abs(aria_exacta - aria_aproximativa) / aria_exacta * 100

cat("aria aproximativa estimata e:", aria_aproximativa, "\n")
cat("aria exactă prin integrare e:", aria_exacta, "\n")
cat("eroarea relativa e (%):", eroare_relativa, "\n")

  

#========= EX II.1 =========
  
a)


integrala_i <- integral(function(x) exp(x), lower = 1, upper = 4)$integral
valoare_exacta_i <- 51.87987

eroare_absoluta_i <- abs(valoare_exacta_i - integrala_i)
eroare_relativa_i <- eroare_absoluta_i / valoare_exacta_i * 100

cat("estimare integrala (e^x) de la 1 la 4:", integrala_i, "\n")
cat("valoarea exacta a integralei e :", valoare_exacta_i, "\n")
cat("eroarea absoluta e :", eroare_absoluta_i, "\n")
cat("eroarea relativa e (%):", eroare_relativa_i, "\n")



b)


integrala_ii <- integral(function(x) 1 / (4 * x^2 - 1), lower = 1, upper = Inf)$integral
valoare_exacta_ii <- log(3/4)

eroare_absoluta_ii <- abs(valoare_exacta_ii - integrala_ii)
eroare_relativa_ii <- eroare_absoluta_ii / valoare_exacta_ii * 100

cat("estimare integrala (1/(4x^2 - 1)) de la 1 la infinit:", integrala_ii, "\n")
cat("valoarea exacta a integralei:", valoare_exacta_ii, "\n")
cat("eroarea absoluta e:", eroare_absoluta_ii, "\n")
cat("eroarea relativa e (%):", eroare_relativa_ii, "\n")





#========= EX II.2  =========
  
  
  
  
set.seed(66)

N <- 50000

lambda <- 3

u_values <- rexp(N, rate = lambda)

f_values <- exp(-2 * u_values^2)

integrala_estimata <- mean(f_values)

integrala_exacta <- sqrt(pi) / 8

cat("valoarea estimata a integralei folosind metoda Monte Carlo imbunatatita e:", integrala_estimata, "\n")
cat("valoarea exacta a integralei e:", integrala_exacta, "\n")
  

#========= EX III.2 =========


# setam seed-ul pentru reproductibilitate
set.seed(69)

# nr de simulari (logic)
num_simulations <- 10000

# parametri pt timpul de servire al fiecarui mecanic
lambda_mecanic1 <- 4
lambda_mecanic2 <- 12

# avem o functie pt a genera timpul de servire al unui mecanic
generare_timp_servire <- function(lambda) {
  return(rexp(1, rate = lambda))
}

# apoi facem o functie pt a determina mecanicul care va servi clientul
mecanic_aleator <- function() {
  prob_mecanic1 <- 3 / 4
  if (runif(1) <= prob_mecanic1) {
    return("mecanic1")
  } else {
    return("mecanic2")
  }
}

# aici simulam timpul de servire pentru fiecare client
timp_servire_total <- numeric(num_simulations)
for (i in 1:num_simulations) {
  mecanic <- mecanic_aleator()
  if (mecanic == "mecanic1") {
    timp_servire_total[i] <- generare_timp_servire(lambda_mecanic1)
  } else {
    timp_servire_total[i] <- generare_timp_servire(lambda_mecanic2)
  }
}
  
# estimam media timpului de servire
medie_timp_servire <- mean(timp_servire_total)
print(medie_timp_servire)




#=========== EX IV.1 ===========

  
# setam seed-ul de reproductibilitate
  set.seed(80)

# parametri pentru variabilele repartizate geometric
parametru_X <- 0.3
parametru_Y <- 0.5

runs <- 10000

# functie pentru generarea unei perechi de valori X,Y
genereaza_pereche_X_Y <- function() {
  X <- rgeom(1, prob = parametru_X)
  Y <- rgeom(1, prob = parametru_Y)
  return(c(X, Y))
}

# simulam perechi de valori X,Y si numaram cate indeplinesc conditia X < Y^2
numar_X_mai_mic_decat_Y_patrat <- 0
for (i in 1:runs) {
  pereche <- genereaza_pereche_X_Y()
  if (pereche[1] < pereche[2]^2) {
    numar_X_mai_mic_decat_Y_patrat <- numar_X_mai_mic_decat_Y_patrat + 1
  }
}

# estimam probabilitatea P(X < Y^2)
probabilitate_estimata <- numar_X_mai_mic_decat_Y_patrat / runs

cat("probabilitatea estimata P(X < Y^2):", probabilitate_estimata, "\n")

# calculam eroarea
E <- 0.005

# estimam nr necesar de simulari pt a obaine o eroare cu probabilitate de 95%
z <- qnorm(0.975) 5%
p <- prob_estimata
numar_necesar_simulari <- (z^2 * p * (1 - p)) / E^2
numar_necesar_simulari <- ceiling(numar_necesar_simulari)

cat("nr necesar de simulari pt o eroare de ±0.005 cu o probabilitate de 95%:", numar_necesar_simulari, "\n")
