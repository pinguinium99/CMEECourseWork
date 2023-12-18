# CMEE 2022 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.
name <- "Prannoy Thazhakaden"
preferred_name <- "Prannoy"
email <- "PVT23@imperial.ac.uk"
username <- "PVT23"

set.seed(7)

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
  
  png(filename="question_8.png", width = 600, height = 400)
  # plot your graph here  
  plot(t,xlab="Generations", ylab="Species Richness")
  Sys.sleep(0.1)
  dev.off()
  
  return("There are no new species being introduced, each neutral step can only reduce the species richness as individuals are replaced")
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
  colnames(df) <- c("Max initial Species","Min initial Species")

  png(filename="question_12.png", width = 600, height = 400)
  # plot your graph here  
  matplot(as.matrix(df[1:2]), type = 'l', col = c('red', 'blue'),xlab="Generations", ylab="Species Richness")
  legend("topright", legend = names(df), col = c('red', 'blue'),lty = 1:2,bty = "n")

  Sys.sleep(0.1)
  dev.off()
  
  return("The maximum number of species in the initial stage drops sharply at the beginning while
         the minimum climbs gradually. As the speciation rate is 0.1 it is much more likely for species
         richness to drop than increase. The species richness then stays around a certain value depending on the 
         speciation rate")
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
      y[(length(y)+1):length(x)] <- 0
    }
    return(x+y)
  }else{
    return(x+y)
  }
}


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
bins[1] <- 1

  
  png(filename="question_16_min.png", width = 600, height = 400)
  # plot your graph here  
  barplot(df2,ylim = c(0,ceiling(max(df2))*1.1),
          main="Minimium initial species richness",
          names.arg = bins,
          xlab="Octave bins",
          ylab="average number of species",
          col="blue"
  )
  Sys.sleep(0.1)
  dev.off()

  png(filename="question_16_max.png", width = 600, height = 400)
  barplot(df1,ylim = c(0,ceiling(max(df1))*1.1),
          main="Maximium initial species richness",
          names.arg = bins,
          xlab="Octave bins",
          ylab="average number of species",
          col="red"
  )
  Sys.sleep(0.1)
  dev.off()
  
  return("The initial state does not matter as we remove the burn in period and after the burn in the system is at a
         dynamic-equilibrium and stays around a constant value.")
}


# Question 17
neutral_cluster_run <- function(speciation_rate=0.1, size=100, wall_time=1, interval_rich=1, interval_oct=10, burn_in_generations=200, output_file_name = "my_test_file_1.rda") {
 
Speciation_min <- init_community_min(size)
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
  com500 <- c(0,0)
  com1000 <- c(0,0)
  com2500 <- c(0,0)
  com5000 <- c(0,0)

  for(i in 1:100){
    name <- paste("HPCoutput_file",i,".rda", sep = "")
    load(file=name) 
    x <- 0
    if(size == 500){
      
      y <- abundance_list[[1]]
      for(j in 1:(length(abundance_list)-1)){
        y <- sum_vect(y,abundance_list[[j+1]])
        x <- x+1
      }
      average <- y/x
      com500 <- sum_vect(com500,average)
    }
    if(size == 1000){
      
      y <- abundance_list[[1]]
      for(j in 1:(length(abundance_list)-1)){
        y <- sum_vect(y,abundance_list[[j+1]])
        x <- x+1
      }
      average <- y/x
      com1000 <- sum_vect(com1000,average)
    }
    if(size == 2500){
      
      y <- abundance_list[[1]]
      for(j in 1:(length(abundance_list)-1)){
        y <- sum_vect(y,abundance_list[[j+1]])
        x <- x+1
      }
      average <- y/x
      com2500 <- sum_vect(com2500,average)
    }
    if(size == 5000){
      
      y <- abundance_list[[1]]
      for(j in 1:(length(abundance_list)-1)){
        y <- sum_vect(y,abundance_list[[j+1]])
        x <- x+1
      }
      average <- y/x
      com5000 <- sum_vect(com5000,average)
    }

  }
  
df1 <- (com500/25) 
df2 <- (com1000/25) 
df3 <- (com2500/25) 
df4 <- (com5000/25)
combined_results <- list(df1,df2,df3,df4) #create your list output here to return  
  # save results to an .rda file
save(combined_results,file = "Neutral_cluster_results.rda")

}

plot_neutral_cluster_results <- function(){

load(file="Neutral_cluster_results.rda")
  df1 <- combined_results[[1]] 
  df2 <- combined_results[[2]] 
  df3 <- combined_results[[3]]
  df4 <- combined_results[[4]]
l1 <- length(df1)
l2 <- length(df2)
l3 <- length(df3)
l4 <- length(df4)
x <- max(l1,l2,l3,l4)
v <- seq(1,x)
vfirst <- 2^(v-1)
vend <- (2^v)-1
bins <- paste(vfirst,"-",vend) # change title fix limits
bins[1] <- 1

    png(filename="plot_neutral_cluster_results.png", width = 600, height = 400)
    par(mfrow = c(2, 2))
    barplot(df1,ylim = c(0,ceiling(max(df1))*1.1),
            main="Starting community of 500 individuals",
            names.arg = bins[1:length(df1)],
            xlab="Octave bins",
            ylab="average number of species",
            col="blue",las=2
    )
    
    barplot(df2,ylim = c(0,ceiling(max(df2))*1.1),
            main="Starting community of 1000 individuals",
            names.arg = bins[1:length(df2)],
            xlab="Octave bins",
            ylab="average number of species",
            col="red",las=2
    )
    barplot(df3,ylim = c(0,ceiling(max(df3))*1.1),
            main="Starting community of 2500 individuals",
            names.arg = bins[1:length(df3)],
            xlab="Octave bins",
            ylab="average number of species",
            col="green",las=2
    )
    barplot(df4,ylim = c(0,ceiling(max(df4))*1.1),
            main="Starting community of 5000 individuals",
            names.arg = bins[1:length(df4)],
            xlab="Octave bins",
            ylab="average number of species",
            col="orange",las=2
    )
    Sys.sleep(0.1)
    dev.off()
    
    return(combined_results)
}


# Question 21
state_initialise_adult <- function(num_stages,initial_size){
 x <- rep(0,num_stages)
 x[num_stages] <- initial_size
 return(x)
}

# Question 22
state_initialise_spread <- function(num_stages,initial_size){
  x <- rep(0,num_stages)
  x[1:num_stages] <- floor(initial_size/num_stages)
  y <- initial_size%%num_stages
  y <- rep(1,y)
  x <- sum_vect(x,y)
  return(x)
}

# Question 23
deterministic_step <- function(state,projection_matrix){
x  <- projection_matrix %*% state
return(x)
}

# Question 24
deterministic_simulation <- function(initial_state,projection_matrix,simulation_length){
  steps <- c(sum(initial_state))
  x <- initial_state
  for(i in 1:simulation_length){
    x <- deterministic_step(x,projection_matrix)
    steps <- append(steps,sum(x))
  }
  return(steps)
}

# Question 25
question_25 <- function(){
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
  
  projection_matrix <- reproduction_matrix + growth_matrix 
  simulation_length <- 24
  x <- deterministic_simulation(state_initialise_spread(4,100),projection_matrix,simulation_length)
  y <- deterministic_simulation(state_initialise_adult(4,100),projection_matrix,simulation_length)
  
  df <- data.frame(cbind(x , y))
  colnames(df) <- c("Spread","Adult")
 
  png(filename="question_25.png", width = 600, height = 400)
  
  matplot(as.matrix(df[1:2]), type = 'l', col = c('red', 'blue'),xlab="Time points(generations)", ylab="Population size")
  legend("topleft", legend = names(df), col = c('red', 'blue'),lty = 1:2,bty = "n")
  
  
  Sys.sleep(0.1)
  dev.off()
  
  return("Having all the individuals in the adult stage leads to a boom in the population size as there are 
         many offspring being produced (2.6 according to the reproduction matrix * 100 Adults) but directly
         after, due to the lower survival probability in the Adult and offspring stages the population
         declines significantly. The spread out state has a more consistent rate of population growth as
         there is much less impact caused by the survival probability in the smaller population size in each stage")
}

# Question 26
multinomial <- function(pool,probs) {
  if(sum(probs)>1){
    print("Sum of probabilities is greater than 1")
  }
  death <- 1 - sum(probs)
  probd <- c(probs,death)
  x <- as.vector(rmultinom(n=1,pool,probd))
  return(x[1:length(probs)])
}

# Question 27
survival_maturation <- function(state,growth_matrix) {
  endstate <- rep(0,length(state))
  for(i in 1:length(state)){
  state2  <- multinomial(state[i],growth_matrix[,i])
  endstate <- sum_vect(endstate,state2)
  }
  return(endstate)
}


# Question 28
random_draw <- function(probability_distribution) {
x  <- sample(1:length(probability_distribution),size = 1,replace = T, prob = probability_distribution)
return(x)
}

# Question 29
stochastic_recruitment <- function(reproduction_matrix,clutch_distribution){
  x <- reproduction_matrix[1,ncol(reproduction_matrix)]
  z <- (sum(clutch_distribution*(1:length(clutch_distribution))))
  y <- x/z
  return(y)# add greater than 1 warning?
}

# Question 30
offspring_calc <- function(state,clutch_distribution,recruitment_probability){
  tot_off <- 0
   x <- state[length(state)]
       num_c <- rbinom(n=1,x,recruitment_probability)
       if(num_c != 0){
       for (i in 1:num_c){
         clutch <- random_draw(clutch_distribution)
         tot_off <- tot_off + clutch
       }
   }
  return(tot_off)
}


# Question 31
stochastic_step <- function(state,growth_matrix,reproduction_matrix,clutch_distribution,recruitment_probability){
  new_state  <- survival_maturation(state,growth_matrix)
  x <- offspring_calc(state, clutch_distribution, recruitment_probability)# ask if new state or state
  new_state[1] <- x+new_state[1]
  return(new_state)
}

# Question 32
stochastic_simulation <- function(initial_state,growth_matrix,reproduction_matrix,clutch_distribution,simulation_length){
  steps <- c(sum(initial_state))
  x <- initial_state
  recruitment_probability <- stochastic_recruitment(reproduction_matrix,clutch_distribution)
  for(i in 1:simulation_length){
    x <- stochastic_step(x,growth_matrix,reproduction_matrix,clutch_distribution,recruitment_probability)
    if(sum(x) == 0){
      return(steps[(length(steps)+1):simulation_length] <- rep(0))
    }else{
      steps <- append(steps,sum(x))
    }
  }
  return(steps)
}

# Question 33
question_33 <- function(){
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
  
  simulation_length <- 24
  x <- stochastic_simulation(state_initialise_spread(4,100),growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  y <- stochastic_simulation(state_initialise_adult(4,100),growth_matrix,reproduction_matrix,clutch_distribution,simulation_length)
  
  df <- data.frame(cbind(x , y))
  colnames(df) <- c("Spread","Adult")

  png(filename="question_33.png", width = 600, height = 400)
  matplot(as.matrix(df[1:2]), type = 'l', col = c('red', 'blue'),xlab="Time points (generations)", ylab="Population size")
  legend("topleft", legend = names(df), col = c('red', 'blue'),lty = 1:2,bty = "n")
  Sys.sleep(0.1)
  dev.off()
  
  return("The stochastic graph is not smooth due to the random chance of individuals moving between stages, producing offspring and dying.
         Additionally the population size in the deterministic graph is continuous allowing for a smoother curve as it does not consider
         whole individuals.")
}

# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){

  smallspread <- 0
  bigspread <- 0
  smalladult <- 0
  bigadult <- 0
  smallspread_ext <- 0
  bigspread_ext <- 0
  smalladult_ext <- 0
  bigadult_ext <- 0
  
  SA<- state_initialise_adult(4,10)
  SS<- state_initialise_spread(4,10)
  BA<- state_initialise_adult(4,100)
  BS<- state_initialise_spread(4,100)

  for(i in 1:100){
    name <- paste("HPCoutput_demographic",i,".rda", sep = "")
    load(file=name) 
    x <- 0
    y <- 0
    if(startval[4] == SS[4]){
      for(j in 1:(length(outputlist))){ 
      if(outputlist[[j]][length(outputlist[[j]])] == 0){
        x <- x+1
      }else{
        y <- y+1
      }
      }      
      smallspread_ext <- x + smallspread_ext
      smallspread <- y + smallspread
    }else{
      if(startval[4] == BS[4]){
        for(j in 1:(length(outputlist))){ 
        if(outputlist[[j]][length(outputlist[[j]])] == 0){
          x <- x+1
        }else{
          y <- y+1
        }
        }
        bigspread_ext <- x + bigspread_ext
        bigspread <- y + bigspread
      }else{
    if(startval[4] == SA[4]){
      for(j in 1:(length(outputlist))){ 
      if(outputlist[[j]][length(outputlist[[j]])] == 0){
        x <- x+1
      }else{
        y <- y+1
      }
      }
      smalladult_ext <- x + smalladult_ext
      smalladult <- y + smalladult
      }else{
      if(startval[4] == BA[4]){
        for(j in 1:(length(outputlist))){ 
        if(outputlist[[j]][length(outputlist[[j]])] == 0){
          x <- x+1
        }else{
          y <- y+1
        }
        }
        bigadult_ext <- x + bigadult_ext
        bigadult <- y + bigadult
      }
      }
      }  
    }
  }

  df1 <- cbind(smallspread,bigspread,smalladult,bigadult)
  df2 <- cbind(smallspread_ext,bigspread_ext,smalladult_ext,bigadult_ext)
  df3 <- rbind(df1,df2)
  
  png(filename="question_36.png", width = 600, height = 400)
  
  barplot(df3[2,]/15000,ylim = c(0,0.05),
          main="Proportion of extinct populations in 15000 simulations",
          ylab = "Proportion",
          xlab="Initial state", col= "darkblue")
  
  Sys.sleep(0.1)
  dev.off()
  
  return("Small population size  was more likely to go extinct overall as there are much fewer individals, the population
         grows much more slowly and thus is at high risk of being wiped out by chance in the early periods.The population
         that was small and spread showed a greater proportion of extinction relative to the small adult-only population. This
         maybe due to the higher possibility of death in the early stages while the adult-only can produce a large number of offspring ")
}


# Question 37
question_37 <- function(){
  
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
  
  projection_matrix <- reproduction_matrix + growth_matrix 
  simulation_length <- 120
  
  determinist_large <- deterministic_simulation(state_initialise_spread(4,100),projection_matrix,simulation_length)
  determinist_small <- deterministic_simulation(state_initialise_spread(4,10),projection_matrix,simulation_length)
  
  smallspread <- c(0,0)
  bigspread <- c(0,0)
  smalladult <- c(0,0)
  bigadult <- c(0,0)
  
  SA<- state_initialise_adult(4,10)
  SS<- state_initialise_spread(4,10)
  BA<- state_initialise_adult(4,100)
  BS<- state_initialise_spread(4,100)

  
  for(i in 1:100){
    name <- paste("HPCoutput_demographic",i,".rda", sep = "")
    load(file=name) 
    x <- 1
    if(startval[4] == SS[4]){
      y <- outputlist[[1]]
      for(j in 2:(length(outputlist))){ 
        y <- sum_vect(y,outputlist[[j]])
        x <- x+1
      }
      average <- y/x
      smallspread <- sum_vect(smallspread,average)
    }else{
    if(startval[4] == BS[4]){
      y <- outputlist[[1]]
      for(j in 2:(length(outputlist))){
        y <- sum_vect(y,outputlist[[j]])
        x <- x+1
      }
      average <- y/x
      bigspread <- sum_vect(bigspread,average)
    }
    }
    if(startval[4] == SA[4]){
      y <- outputlist[[1]]
      for(j in 2:(length(outputlist))){ 
        y <- sum_vect(y,outputlist[[j]])
        x <- x+1
      }
      average <- y/x
      smalladult <- sum_vect(smalladult,average)
    }else{
      if(startval[4] == BA[4]){
        y <- outputlist[[1]]
        for(j in 2:(length(outputlist))){
          y <- sum_vect(y,outputlist[[j]])
          x <- x+1
        }
        average <- y/x
        bigadult <- sum_vect(bigadult,average)
      }
    }
  }
  
 smallspread <- smallspread/25 
 bigspread <- bigspread/25
 smalladult <- smalladult/25
 bigadult<- bigadult/25
 
  
  df1 <- data.frame(cbind(bigspread, determinist_large))
  df2 <- data.frame(cbind(smallspread,determinist_small))
  colnames(df2) <- c("Stochastic","Deterministic")
  colnames(df1) <- c("Stochastic","Deterministic")
  
  
  png(filename="question_37_small.png", width = 600, height = 400)
  
  matplot(c(df2$Stochastic/df2$Deterministic),ylim = c(0.8,1.2), type = 'l', col = c('red', 'black'),main = ("Deviation between stochastic and deterministic simulations for small spread initial state"),xlab="Time points (generations)", ylab="Deviation (Stochastic/Deterministic)")
  legend("topleft", legend = c("Deviation","y = 1"), col = c('red', 'black'),lty = 1:2,bty = "n")
  lines(c(1:121),y = c(rep(1,121)),col = 'black',lty = 2)

  Sys.sleep(0.1)
  dev.off()
  ?plot
  png(filename="question_37_large.png", width = 600, height = 400)
  
  
  matplot(c(df1$Stochastic/df1$Deterministic),ylim = c(0.8,1.2), type = 'l', col = c('red', 'black'),main = ("Deviation between stochastic and deterministic simulations for large spread initial state"),xlab="Time points (generations)", ylab="Deviation (Stochastic/Deterministic)")
  legend("topleft", legend = c("Deviation","y = 1"), col = c('red', 'black'),lty = 1:2,bty = "n")
  lines(c(1:121),y = c(rep(1,121)),col = 'black',lty = 2)
  
  Sys.sleep(0.1)
  dev.off()
  
  return("Starting with 100 population would provide a better average for comparing to the deterministic model as it would reduce
         the possibility of the population crashing and having more individuals reduces the chance of drastic shifts 
         in population size due to random chance")
}

  

# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  
  speciation_rate <- 0.1
  community <- 100
  generation_lap <-20 
  fullgeneration <- 2000
  sim <- 100
  speciationmin_sum <- rep(NA,fullgeneration+1)
  speciationmax_sum <- rep(NA,fullgeneration+1)
  confidenceint <- rep(NA,fullgeneration+1)
  for (i in 1:sim) {
    Speciation_max <- neutral_time_series_speciation (init_community_max(community),speciation_rate,fullgeneration)
    Speciation_min <- neutral_time_series_speciation (init_community_min(community),speciation_rate,fullgeneration)
    speciationmin_sum <- (Speciation_min + speciationmin_sum )
    speciationmax_sum <- (Speciation_max + speciationmax_sum )
  }
  df <- data.frame(cbind(speciationmax_sum/sim , speciationmin_sum/sim))

  
  png(filename="Challenge_A_min.png", width = 600, height = 400)
  
  matplot(as.matrix(df[1:2]), type = 'l', col = c('red', 'blue'),xlab="Generations", ylab="Species Richness")
  legend("topright", legend = c("Max Richness", "Min Richness" ), col = c('red', 'blue'),lty = 1:2,bty = "n")
  
  
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

init_community_random <- function(rich,size){
  x <- seq(1,rich,1)
  species <- rep(x,length = size) 
  return(species)
}
# Challenge question B
Challenge_B <- function() {
  x <- list()
  for (i in 1:100){
  x[[i]]  <- species_richness(init_community_random(i,100))
  imax[[i]] <- init_community_random(i,100)
  print(imax)
  
  }
 
  for (i in 1:length(imax)) {
    for(j in 1:2200){
    imax[[i]] <- neutral_generation_speciation(imax[[i]], 0.1)
    if (j %% 20 == 0){
      x[[i]][length(x[[i]])+1] <- species_richness(imax[[i]])
    }
  }
}

  
  png(filename="Challenge_B.png", width = 600, height = 400)
  
  plot(seq(1, length(x[[1]]), 1), x[[1]], type = 'l', ylim = c(0,100))
  
  # Add the others
  for (i in 2:length(x)) {
    lines(seq(1, length(x[[i]]), 1), x[[i]])
  }  
  
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

coalescence_step <- function(j,v){
  x <- rep(1,j)
  abundances <- c()
  N <- j
  o <- v*((j-1)/(1-v))
  while (N > 1) {
    indexj <- sample(c(1:length(x)),1)
    randnum <- runif(1,min = 0, max = 1)
    if(randnum < (o/(o+(N-1)))){
      abundances <- append(abundances,x[indexj])
    }else{
      minusj <- (1:length(x))
      indexi <- sample(minusj[-indexj],1)
      x[indexi] <- x[indexi]+x[indexj]
    }
    x <- x[-indexj]
    N <- N - 1
  }
  if(N == 1){
    abundances <- append(abundances,x)
  }
  
  return(abundances)
}

# Challenge question D
Challenge_D <- function() {
  com500 <- c()
  for (i in 1:1000){
  x  <- coalescence_step(500,0.003676)
  com500 <- sum_vect(com500 ,octaves(x))
  }
  com1000 <- c()
  for (i in 1:1000){
    x  <- coalescence_step(1000,0.003676)
    com1000 <- sum_vect(com1000 ,octaves(x))
  }
  com2500 <- c()
  for (i in 1:1000){
    x  <- coalescence_step(2500,0.003676)
    com2500 <- sum_vect(com2500 ,octaves(x))
  }
  com5000 <- c()
  for (i in 1:1000){
    x  <- coalescence_step(5000,0.003676)
    com5000 <- sum_vect(com5000 ,octaves(x))
  }
  df1 <- com500/1000
  df2 <- com1000/1000
  df3 <- com2500/1000
  df4 <- com5000/1000
  l1 <- length(df1)
  l2 <- length(df2)
  l3 <- length(df3)
  l4 <- length(df4)
  x <- max(l1,l2,l3,l4)
  v <- seq(1,x)
  vfirst <- 2^(v-1)
  vend <- (2^v)-1
  bins <- paste(vfirst,"-",vend) # change title fix limits
  bins[1] <- 1
  
  png(filename="Challenge_D.png", width = 600, height = 400)
  
  par(mfrow = c(2, 2))
  barplot(df1,ylim = c(0,ceiling(max(df1))*1.1),
          main="Starting community of 500 individuals (coalescence)",
          names.arg = bins[1:length(df1)],
          xlab="Octave bins",
          ylab="average number of species",
          col="blue",las=2
  )
  
  barplot(df2,ylim = c(0,ceiling(max(df2))*1.1),
          main="Starting community of 1000 individuals (coalescence)",
          names.arg = bins[1:length(df2)],
          xlab="Octave bins",
          ylab="average number of species",
          col="red",las=2
  )
  barplot(df3,ylim = c(0,ceiling(max(df3))*1.1),
          main="Starting community of 2500 individuals (coalescence)",
          names.arg = bins[1:length(df3)],
          xlab="Octave bins",
          ylab="average number of species",
          col="green",las=2
  )
  barplot(df4,ylim = c(0,ceiling(max(df4))*1.1),
          main="Starting community of 5000 individuals (coalescence)",
          names.arg = bins[1:length(df4)],
          xlab="Octave bins",
          ylab="average number of species",
          col="orange",las=2)
          
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


