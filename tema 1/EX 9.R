EX.9

plot_poisson_prob <- function(lambda, n)
{
  x <- 0:(n-1)
  
  #Calculte poisson probabillities
  prob <- dpois(x, lambda)
  
  #Print probabillities
  cat("Poisson Probabilities:\n")
  for (i in 1:n)
    {
    cat("P(", i-1, ") = ", prob[i], "\n")
    }
  
  #Plot the probabillities
  barplot(prob, names.arg = 0:(n-1), xlab = "x", ylab = "Probabillity", main = "Poisson probabillity function")
}

#Examples
lambda <- 3
n <- 10
plot_poisson_prob(lambda, n)
