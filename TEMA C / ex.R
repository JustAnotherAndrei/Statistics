#EX C1

#a)
generate_random_permutation <- function(n)
{
  U <- runif(n) 
  permutation <- order(U) 
  return(permutation)
}

set.seed(91)  
print(generate_random_permutation(5))


#b)
compare_lexicographic <- function(Wi, Wj)
  {
  Lij <- min(nchar(Wi), nchar(Wj))
  for (l in 1:Lij) {
    if (substr(Wi, l, l) != substr(Wj, l, l))
    {
      return(substr(Wi, l, l) < substr(Wj, l, l))
    }
  }
  
  if (nchar(Wi) == nchar(Wj))
  {
    repeat 
    {
      random_bit_Wi <- sample(c("0", "1"), 1)
      random_bit_Wj <- sample(c("0", "1"), 1)
      Wi <- paste0(Wi, random_bit_Wi)
      Wj <- paste0(Wj, random_bit_Wj)
      if (Wi != Wj) 
      {
        return(Wi < Wj)
      }
    }
  } 
  else 
  {
    return(nchar(Wi) < nchar(Wj))
  }
}

Wi <- "101"
Wj <- "100"
print(compare_lexicographic(Wi, Wj)) 


#c)
random_quick_sort <- function(words)
{
  if (length(words) <= 1) 
  {
    return(words)
  }
  pivot <- sample(words, 1)
  less <- words[sapply(words, function(x) compare_lexicographic(x, pivot))]
  equal <- words[sapply(words, function(x) x == pivot)]
  greater <- words[sapply(words, function(x) compare_lexicographic(pivot, x))]
  
  return(c(random_quick_sort(less), equal, random_quick_sort(greater)))
}

words <- c("101", "100", "111", "010", "001")
sorted_words <- random_quick_sort(words)
print(sorted_words)


#d)
generate_random_permutation_words <- function(n, k)
{
  words <- replicate(n, paste0(sample(c("0", "1"), k, replace = TRUE), collapse = ""))
  sorted_indices <- order(random_quick_sort(words))
  return(sorted_indices)
}

set.seed(42)  
print(generate_random_permutation_words(5, 3))





#EX C2

#a)
determine_cut <- function(nodes, edges) 
{
  n <- length(nodes) %/% 2 
  A <- sample(nodes, n)  
  B <- setdiff(nodes, A)
  
  cut_edges <- edges[sapply(1:nrow(edges), function(i)
  {
    (edges[i, 1] %in% A && edges[i, 2] %in% B) || (edges[i, 1] %in% B && edges[i, 2] %in% A)
  }), ]
  
  return(nrow(cut_edges))
}

set.seed(68)  
V <- 1:7
E <- matrix(c(1,2, 1,3, 2,4, 3,4, 4,5, 5,6, 5,7, 6,7), ncol=2, byrow=TRUE)
max_cut_cardinality <- determine_cut(V, E)
print(max_cut_cardinality)


#b)
find_best_cut <- function(nodes, edges, trials = 1000) 
{
  best_cut_size <- 0
  for (i in 1:trials) 
  {
    cut_size <- determine_cut(nodes, edges)
    if (cut_size > best_cut_size) 
    {
      best_cut_size <- cut_size
    }
  }
  return(best_cut_size)
}

best_cut_cardinality <- find_best_cut(V, E, trials = 1000)
print(best_cut_cardinality)

