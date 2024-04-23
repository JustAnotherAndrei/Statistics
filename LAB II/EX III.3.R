outliers_iqr = function(x)
{
  q_1 = as.vector(quantile(x))[2]
  q_3 = as.vector(quantile(x))[4]
  iqr = q_3 - q_1
  
  outliers = vector()
  k = 0
  for (i in 1:length(x))
  {
    if (x[i] < q_1 - 1.5 * iqr | x[i] > q_3 + 1.5 * iqr)
    {
      k = k + 1
      outliers[k] = x[i]
    }
  }
  return(outliers)
}

outliers_mean = function(x)
{
  m = mean(x)
  s = sd(x)
  
  outliers = vector()
  k = 0
  for (i in 1:length(x))
  {
    if (x[i] < m-2*s | x[i] > m+2*s)
    {
      k = k + 1
      outliers[k] = x[i]
    }
  }
  return(outliers)
}


exIII3 = function()
{
  summary(sample)
  outliers_mean(sample)
  outliers_iqr(sample)
}