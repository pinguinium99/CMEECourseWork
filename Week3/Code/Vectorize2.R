# Runs the stochastic Ricker equation with gaussian fluctuations


rm(list = ls())

stochrick <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2,numyears = 100)
{

  N <- matrix(NA, numyears, length(p0))  #initialize empty matrix

  N[1, ] <- p0

  for (pop in 1:length(p0)) { #loop through the populations

    for (yr in 2:numyears){ #for each pop, loop through the years

      N[yr, pop] <- N[yr-1, pop] * exp(r * (1 - N[yr - 1, pop] / K) + rnorm(1, 0, sigma)) # add one fluctuation from normal distribution
    
     }
  
  }
 return(N)

}

print(system.time(res2<-stochrick()))


# Now write another function called stochrickvect that vectorizes the above to
# the extent possible, with improved performance: 

stochrickvect <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2,numyears = 100)
{
  
  N <- matrix(NA, numyears, length(p0))  #initialize empty matrix
  
  N[1, ] <- p0
  
  for (yr in 2:numyears){ #for each pop, loop through the years
      
      N[yr,] <- N[yr-1,] * exp(r * (1 - N[yr - 1,] / K) + rnorm(1, 0, sigma)) # add one fluctuation from normal distribution
      
  }
    
  return(N)
  
}
  
  
# print("Vectorized Stochastic Ricker takes:")
print(system.time(res2<-stochrickvect()))


############# ignore below #########


stochrickvect2 <- function(p0 = runif(1000, .5, 1.5), r = 1.2, K = 1, sigma = 0.2, numyears = 100) {
  
  growth_function <- function(population) {
    N_pop <- numeric(numyears)
    N_pop[1] <- population
    
    for (yr in 2:numyears) {
      growth_factor <- r * (1 - N_pop[yr - 1] / K)
      stochastic_term <- rnorm(1, 0, sigma)
      N_pop[yr] <- N_pop[yr-1] * exp(growth_factor + stochastic_term)
    }
    
    return(N_pop)
  }
  
  N_list <- lapply(p0, growth_function)
  N <- do.call(cbind, N_list)
  
  return(N)
}

print(system.time(res2<-stochrickvect2()))



