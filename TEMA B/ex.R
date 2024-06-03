#EX B1

estimate_torus_volume <- function(R, r, num_samples) 
{

  x1 <- runif(num_samples, -R, R)
  x2 <- runif(num_samples, -R, R)
  x3 <- runif(num_samples, -r, r)
  
  inside_torus <- x3^2 + (sqrt(x1^2 + x2^2) - R)^2 < r^2
  
  proportion_inside <- mean(inside_torus)
  
  volume_cube <- (2 * R) * (2 * R) * (2 * r)
  
  estimated_volume <- proportion_inside * volume_cube
  
  return(estimated_volume)
}

R <- 10
r <- 3

sample_sizes <- c(10000, 20000, 50000)

exact_volume <- 2 * pi^2 * R * r^2

results <- data.frame(
  Sample_Size = integer(),
  Estimated_Volume = numeric(),
  Relative_Error = numeric()
)

for (num_samples in sample_sizes)
{
  estimated_volume <- estimate_torus_volume(R, r, num_samples)
  relative_error <- abs(estimated_volume - exact_volume) / exact_volume
  results <- rbind(results, data.frame(Sample_Size = num_samples, Estimated_Volume = estimated_volume, Relative_Error = relative_error))
}

print(results)



#EX B2



estimate_triangle_area <- function(num_samples)
{
  
  a <- 0
  b <- 2
  c <- 0
  d <- 3
  
  x <- runif(num_samples, a, b)
  y <- runif(num_samples, c, d)
  
  inside_triangle <- (y >= 0) & (y <= 2 * x) & (y <= 6 - 3 * x)
  
  proportion_inside <- mean(inside_triangle)
  
  area_rectangle <- (b - a) * (d - c)
  
  estimated_area <- proportion_inside * area_rectangle
  
  return(estimated_area)
}

num_samples <- 20000

estimated_area <- estimate_triangle_area(num_samples)

cat("Aria estimata a triunghiului e:", estimated_area, "\n")



#EX B3

#a)

f_a <- function(x)
{
  (2*x - 1) / (x^2 - x - 6)
}

integral_a <- integrate(f_a, lower = -1, upper = 1)
cat("Integrala a) estimata e:", integral_a$value, "\n")
cat("Valoarea exacta a integralei (a) e:", log(3) - log(2), "\n")

#b)

f_b <- function(x)
{
  (x + 4) / (x - 3)^(1/3)
}

integral_b <- integrate(f_b, lower = 3 + 1e-6, upper = 11)
cat("Integrala b) estimata e:", integral_b$value, "\n")
cat("Valoarea exacta a integralei (b) e: 68.8\n")

#c)

f_c <- function(x) 
{
  x * exp(-x^2)
}

integral_c <- integrate(f_c, lower = 0, upper = Inf)
cat("Integrala c) estimata e:", integral_c$value, "\n")
cat("Valoarea exacta a integralei (c) e:", 1/2, "\n")

#EX B4 

#a)

initial_users <- 10000
n <- 1000
p <- 0.25
q <- 0.01


simulate_years_until_target <- function(initial_users, n, p, q, target_users)
{
  years <- 0
  current_users <- initial_users
  
  while (current_users < target_users) 
  {
    new_users <- rbinom(1, n, p)
    retained_users <- rbinom(1, current_users, 1 - q)
    current_users <- retained_users + new_users
    years <- years + 1
  }
  
  return(years)
}

set.seed(123) #pt reproductibilitate
years_to_target <- replicate(num_simulations, simulate_years_until_target(initial_users, n, p, q, target_users))

mean_years <- mean(years_until_target)
cat("Nr mediu de ani pana la atingerea targetului de 15000 de utilizatori e:", mean_years, "\n")
