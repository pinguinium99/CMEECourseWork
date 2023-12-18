rm(list=ls())

name <- "Prannoy Thazhakaden"
preferred_name <- "Prannoy"
email <- "PVT23@imperial.ac.uk"
username <- "PVT23"

source("pvt23_HPC_2023_main.R")
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) 
set.seed(iter)
community <- c(500, 1000, 2500, 5000)
speciation <- 0.003676
filename <- paste0("HPCoutput_file",iter,".rda")
neutral_cluster_run(speciation_rate=speciation, size = community[iter %% 4+1], wall_time=(11.5*60), interval_rich=1, interval_oct= community[iter %% 4+1]/10, burn_in_generations=community[iter %% 4+1]*8, output_file_name = filename)
  
