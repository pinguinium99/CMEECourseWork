rm(list=ls())

name <- "Prannoy Thazhakaden"
preferred_name <- "Prannoy"
email <- "PVT23@imperial.ac.uk"
username <- "PVT23"

source("pvt23_HPC_2023_main.R")
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) 

set.seed(iter)

SA<- state_initialise_adult(4,10)
SS<- state_initialise_spread(4,10)
BA<- state_initialise_adult(4,100)
BS<- state_initialise_spread(4,100)

startstate <- list(SA, SS, BA, BS)

startval <- startstate[[iter %% 4+1]]

clutch_distribution <- c(0.06,0.08,0.13,0.15,0.16,0.18,0.15,0.06,0.03) 
growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0, 
                          
                          0.5, 0.4, 0.0, 0.0, 
                          
                          0.0, 0.4, 0.7, 0.0, 
                          
                          0.0, 0.0, 0.25, 0.4), 
                        
                        nrow=4, ncol=4, byrow=T) 

reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6, 
                                
                                0.0, 0.0, 0.0, 0.0, 
                                
                                0.0, 0.0, 0.0, 0.0, 
                                
                                0.0, 0.0, 0.0, 0.0), 
                              
                              nrow=4, ncol=4, byrow=T) 

filename <- paste0("HPCoutput_demographic",iter,".rda")
simulation_length <- 120
outputlist <- list()
for (i in 1:150){
  x <- stochastic_simulation(startval,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  outputlist[[i]]<- x
}
save(outputlist,startval,file = filename)

