plot_geometric_prob <- function(p, n) {
  #Generate x values from 1 to n (the first n outcomes of the geometric distribution)
  x <- 1:n
  
  #Calculate geometric probabillities
  prob <- dgeom(x, p)
  
  #Print probabillities
  cat("Geometric Probabillities:\n")
  for (i in 1:n) {
    cat("P(", i, ") = ", probs[i], "\n")
  }
  
  #Plot probabillities
  barplot(prob, names.arg = 1:n, xlab = "x", ylab = "Probabillity", main = "Geometric probabillity function")
}

#Examples
p <- 0.3
n <- 10
plot_geometric_prob(p, n)
