rm(list=ls())

load("../Data/KeyWestAnnualMeanTemperature.RData")

ls()

class(ats)

head(ats)

plot(ats)
?cor()
corats <- cor(ats$Temp,ats$Year)

samp_cor <- function(x){
  year <- sample(x$Year, size = 100, replace = TRUE)
  temp <- sample(x$Temp, size = 100, replace = TRUE)
  randomsamp <- as.data.frame(cbind(year,temp))
  corrand <- cor(randomsamp$temp,randomsamp$year)
  return(corrand)
}

# possible shorter alternative to collect random samples

# N = 10000
# corr = c()
# for (i in 1:N) {
#   examplefile = transform(ats, Temp = sample(Temp))
#   corr[i] = cor(examplefile$Temp, examplefile$Year)
# }





d <- rep(NA,10000)
Random_cor <- (replicate(10000, samp_cor(ats), simplify=T))
d <- as.data.frame(Random_cor)

library(ggplot2)

ggplot(d, aes(f,)) +
  geom_density() +
  xlim(-1,1)+
  geom_vline(xintercept = corats)

print(sum(d$Random_cor > corats)/length(d$Random_cor))

# need to write the latex file to explain results