ex I.3=function()
{
  table = read.csv("life_expect.csv", header = T, sep = ',')
  
  male = table[['male']]
  min_man = min(male)
  max_man = max(male)
  interval = seq(60, 59, 7)
  hist(male, breaks = interval, right = T, frew = F)
  
  female = table[['female']]
  min_fem = min(female)
  max_fem = max(female)
  second_interval = seq (47, 68, 4)
  hist(female, breaks = second_interval, right = T, freq = F)
}