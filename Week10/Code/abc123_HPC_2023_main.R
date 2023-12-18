# CMEE 2022 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.
name <- "Prannoy Thazhakaden"
preferred_name <- "Prannoy"
email <- "PVT23@imperial.ac.uk"
username <- "PVT23"

# Please remember *not* to clear the workspace here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  richness <- length(unique(community))
  return(richness)
}

# Question 2
init_community_max <- function(size){
  species <- seq(1,size,1)
  return(species)
}

# Question 3
init_community_min <- function(size){
  minspecies <- rep(1,length = size) 
  return(minspecies)
}

# Question 4
choose_two <- function(max_value){
  x <- sample(1:max_value ,2 , replace = F)
  return(x)
}

# Question 5
neutral_step <- function(community){
  x <- choose_two(length(community))
  community[x[1]] = community[x[2]]
  return(community)
}

# Question 6
neutral_generation <- function(community){
  generation <- round(jitter(length(community)/2,amount = 0.1))
  for(i in 1:generation){
    community <- neutral_step(community)
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration){
  tab <- rep(NA,length(duration)+1)
  x <- species_richness(community)
  for (i in 1:duration) {
    tab[1] <- x
    community <- neutral_generation(community)
    tab[i + 1] <- species_richness(community)
  }
  return(tab)
}

# Question 8
question_8 <- function(){
  t <- neutral_time_series(init_community_max(100),200)
  plot(t,xlab="Generations", ylab="Species Richness")
  
  png(filename="question_8", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("There are no new species being introduced, each neutral step can only reduce the species richness")
}

# Question 9
neutral_step_speciation <- function(community,speciation_rate){
  z <- max(community)
  x <- runif(1,0,1)
  if(x > speciation_rate){
    y <- choose_two(length(community))
    community[y[1]] = community[y[2]]
    return(community)
  }else{
    y <- choose_two(length(community))
    community[y[1]] = z+1
    return(community)
  }
  
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  
    generation <- round(jitter(length(community)/2,amount = 0.1))
    for(i in 1:generation){
      community <- neutral_step_speciation(community,speciation_rate)
    }
    return(community)
  
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration){
  tab <- rep(NA,length(duration)+1)
  x <- species_richness(community)
  for (i in 1:duration) {
    tab[1] <- x
    community <- neutral_generation_speciation(community,speciation_rate)
    tab[i + 1] <- species_richness(community)
  }
  return(tab)
}

# Question 12
question_12 <- function()  {  
  Speciation_max <- neutral_time_series_speciation (init_community_max(100),0.1,200)
  Speciation_min <- neutral_time_series_speciation (init_community_min(100),0.1,200)
  df <- data.frame(cbind(Speciation_max , Speciation_min ))
  
  matplot(as.matrix(df[1:2]), type = 'l', col = c('red', 'blue'),xlab="Generations", ylab="Species Richness")
  legend("topright", legend = names(df), col = c('red', 'blue'),lty = 1:2,bty = "n")

  png(filename="question_12", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 13
species_abundance <- function(community)  {
  tab <- (table(community))
  return(sort(as.numeric(tab), decreasing = T))
}

# Question 14
octaves <- function(abundance_vector) {
  x <- floor(log2(abundance_vector)+1)
  y <- tabulate(x)
  return(y)
}

# Question 15
sum_vect <- function(x, y){
  if (length(x) != length(y)){
    if (length(x) < length(y)){
      x[(length(x)+1):length(y)] <- 0
    }else{
      print(length(y))
      print(length(x))
      y[(length(y)+1):length(x)] <- 0
    }
    return(x+y)
  }else{
    return(x+y)
  }
}

sam_vect <- function(x,y){
  max_length <- max(length(x),length(y))
  x <- c(x, rep(0, max_length - length(x)))
  y <- c(y, rep(0, max_length - length(y)))
  return(x + y)
}

library(microbenchmark)
test1 <- sample(1:9, 1000000, replace = TRUE)
test2 <- sample(1:9, 1000000, replace = TRUE)
microbenchmark(
  sum_vect(test1,test2),
  sam_vect(test1,test2)
)

# Question 16 
question_16 <- function() {
  speciation_rate <- 0.1
  community <- 100
  generation_lap <-20 
  fullgeneration <- 2000
  Speciation_max <- init_community_max(community)
  Speciation_min <- init_community_min(community)
  for(i in 1:200){
    Speciation_max <- neutral_generation_speciation(Speciation_max,speciation_rate)
    Speciation_min <- neutral_generation_speciation(Speciation_min,speciation_rate)
  }
  mincomm <- c(0,0)
  maxcomm <- c(0,0)
  mincomm <- sum_vect(octaves(species_abundance(Speciation_min)),mincomm)
  maxcomm <- sum_vect(octaves(species_abundance(Speciation_max)),maxcomm)

  for(i in 1:fullgeneration){
    Speciation_max <- neutral_generation_speciation(Speciation_max,speciation_rate)
    Speciation_min <- neutral_generation_speciation(Speciation_min,speciation_rate)
    if(i %% generation_lap == 0){
      mincomm <- sum_vect(octaves(species_abundance(Speciation_min)),mincomm)
      maxcomm <- sum_vect(octaves(species_abundance(Speciation_max)),maxcomm)
    }
  }
  df1 <- (maxcomm/101)
  df2 <- (mincomm/101)
  l1 <- length(df1)
  l2 <- length(df2)
  x <- max(l1,l2)
  v <- seq(1,x)
  vfirst <- 2^(v-1)
  vend <- (2^v)-1
  bins <- paste(vfirst,"-",vend) # change title fix limits

  barplot(df2,
          main="Octaves min",
          names.arg = bins,
          xlab="Octave bins",
          ylab="Mean Species Abundances",
          col="blue"
  )
  
  png(filename="question_16_min", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  barplot(df1,
          main="Octaves max",
          names.arg = bins,
          xlab="Octave bins",
          ylab="Mean Species Abundances",
          col="red"
  )
  png(filename="question_16_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 17
neutral_cluster_run <- function(speciation_rate=0.1, size=100, wall_time=10, interval_rich=1, interval_oct=10, burn_in_generations=200, output_file_name = "my_test_file_1.rda") {
 
Speciation_min <- init_community_min(100)
time_series <- c(species_richness(Speciation_min))
i <- 0
x <- 0
abundance_list <- list()
tm <- proc.time()[[3]]
while (proc.time()[[3]] - tm < (wall_time*60))#*60
{
  i <- i + 1 
  Speciation_min <- neutral_generation_speciation(Speciation_min,speciation_rate)
    if(i %% interval_rich == 0 & i < burn_in_generations){
      time_series <- append(time_series,species_richness(Speciation_min))
    }
  if(i %% interval_oct == 0 & i > burn_in_generations){
    x <- x+1
    abundance_list[[x]]  <- octaves(species_abundance(Speciation_min))
  }
}
  
  community <- Speciation_min
  total_time <- proc.time()[[3]] -tm
  save(time_series, abundance_list,community,total_time, speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations,file = output_file_name)
}


# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
process_neutral_cluster_results <- function() {
  
  
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_neutral_cluster_results <- function(){

    # load combined_results from your rda file
  
  
  
    png(filename="plot_neutral_cluster_results", width = 600, height = 400)
    # plot your graph here
    Sys.sleep(0.1)
    dev.off()
    
    return(combined_results)
}


# Question 21
state_initialise_adult <- function(num_stages,initial_size){
  
}

# Question 22
state_initialise_spread <- function(num_stages,initial_size){
  
}

# Question 23
deterministic_step <- function(state,projection_matrix){
  
}

# Question 24
deterministic_simulation <- function(initial_state,projection_matrix,simulation_length){
  
}

# Question 25
question_25 <- function(){
  
  png(filename="question_25", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 26
multinomial <- function(pool,probs) {
  
}

# Question 27
survival_maturation <- function(state,growth_matrix) {
  
}

# Question 28
random_draw <- function(probability_distribution) {
  
}

# Question 29
stochastic_recruitment <- function(reproduction_matrix,clutch_distribution){
  
}

# Question 30
offspring_calc <- function(state,clutch_distribution,recruitment_probability){
  
}

# Question 31
stochastic_step <- function(state,growth_matrix,reproduction_matrix,clutch_distribution,recruitment_probability){
  
}

# Question 32
stochastic_simulation <- function(initial_state,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length){
  
}

# Question 33
question_33 <- function(){
  
  
  
  png(filename="question_33", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){
  
  png(filename="question_36", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 37
question_37 <- function(){
  
  png(filename="question_37_small", width = 600, height = 400)
  # plot your graph for the small initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_37_large", width = 600, height = 400)
  # plot your graph for the large initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}



# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  
  
  
  png(filename="Challenge_A_min", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question B
Challenge_B <- function() {
  
  
  
  png(filename="Challenge_B", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question C
Challenge_C <- function() {
  
  
  
  png(filename="Challenge_C", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function(){
  
  
  
  png(filename="Challenge_E", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function(){
  
  
  
  png(filename="Challenge_F", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}
