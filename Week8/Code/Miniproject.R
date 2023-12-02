data <- read.csv("../Data/LogisticGrowthData.csv", header = T)
data
metadat <- read.csv("../Data/LogisticGrowthMetaData.csv", header = F)

metadat


library(dplyr)
library(ggplot2)
library(broom)
library(minpack.lm)
library(gridExtra)
library(tidyverse)

set.seed(011223)

data$id <- paste(data$Species, data$Medium, data$Temp, data$Citation)


data <- data %>% group_by(id) %>% mutate(No_id = cur_group_id())

datsplit <- data  %>%  group_by(No_id)  %>% group_split()

datsplit[285]

write.csv(data, "../Data")



AIC_fun <- function(x){
  gl <- glance(x)
  n <- gl$nobs
  RSS_x <-sum(residuals(x)^2)
  px <- length(coef(x))#num of parameters
  AIC_x <- n + 2 + n * log((2 * pi) / n) + n * log(RSS_x) + 2 * px
  return(AIC_x)
}


RSq_fun <- function(x,dat){
  RSS <-sum(residuals(x)^2) # Residual sum of squares
  TSS <-sum((dat$PopBio - mean(dat$PopBio))^2) #total sum of squares
  RSq <- 1 - (RSS/TSS) #R squared value
  return(RSq)
}


AICc_fun <- function(x){
  gl <- glance(x)
  n <- gl$nobs
  RSS_x <-sum(residuals(x)^2)
  px <- length(coef(x))#num of parameters
  AIC_x <- n + 2 + n * log((2 * pi) / n) + n * log(RSS_x) + 2 * px + (2 * px * (px + 1)/ n - px - 1)
  return(AIC_x)
}

AICc_fun <- function(x){
  gl <- glance(x)
  n <- gl$nobs
  RSS_x <-sum((residuals(x)^2))
  px <- length(coef(x))#num of parameters
  AIC_x <- n + 2 + n * log((2 * pi) / n) + n * log(RSS_x) + 2 * px + (2 * px * (px + 1)/ n - px - 1)
  return(AIC_x)
}



startvcalc <- function(x){
  datax <- data[data$No_id == x & data$PopBio > 0,]
  startingval <- matrix(NA,100,3) 
  N_0_start <- min(datax$PopBio)
  k_start <- max(datax$PopBio)
  mint <- datax$Time[datax$PopBio == N_0_start]
  maxt <- datax$Time[datax$PopBio == k_start]
  lm_growth <- lm(PopBio ~ Time, data = datax[datax$Time > mint & datax$Time < maxt,])
  r_max_start <- lm_growth$coefficients[2]
  
  startingval[,1] <- N_0_start
  startingval[,2] <- k_start
  startingval[,3] <- rnorm(100, mean = r_max_start, sd = 1)
  return(startingval)
  
}



logistic_model <- function(t, r_max, k, N_0) {
  return(N_0 * k * exp(r_max * t) / (k + N_0 * (exp(r_max * t) - 1)))
}

logisticbf <- function(x) {
  datax <- data[data$No_id == x & data$PopBio > 0,]
  strtval <- startvcalc(x)
  fits <- list()  
  endingval <- matrix(NA,100,6)
  for (i in 1:nrow(strtval)) {
    result <- try({
      fit_logistic <- nlsLM(log(PopBio) ~ logistic_model(t = Time, r_max, k, N_0), 
                            data = datax,
                            start = list(r_max = strtval[i, 3], N_0 = strtval[i, 1], k = strtval[i, 2]),
                            control = nls.lm.control(maxiter = 10000))
      fit_logistic
    }, silent = TRUE)
    if (!inherits(result, "try-error")) {
      print(result)
      endingval[i,4] <- AICc_fun(result)
      endingval[i,5] <- RSq_fun(result,datax)
      endingval[i,1] <- coef(result)["r_max"]
      endingval[i,2] <- coef(result)["N_0"]
      endingval[i,3] <- coef(result)["k"]
      endingval[i,6] <- datax$No_id[1] 
    } else {
      warning(paste("Error in iteration", i, "- skipping."))
    }
    min_aic_row <- as.vector(endingval[which.min(endingval[,4]),]) 
    
    
  }
  return(min_aic_row)
}


######################


logisticbfmod2<- function(x) {
  bf <- data.frame(matrix(ncol = 5, nrow = 0))
  
  
  unique_ids <- unique(x$No_id)
  
  for (j in unique_ids) {
    result <- tryCatch({
      bf <- rbind(bf,logisticbf(j))
    }, error = function(e) {
      warning(paste("Error for No_id", j, "- skipping. Error message:", e$message))
      NULL
    })
  }
  colnames(bf) <- c("R_max", "N_0", "k", "AIC", "RSq","ID")
  return(bf)
}


results1 <- logisticbfmod2(data)


startvcalc2 <- function(x){
  datax <- data[data$No_id == x & data$PopBio > 0,]
  datax$logPopBio <- log(datax$PopBio)
  startingval <- matrix(NA,100,4) 
  N_0_start <- min(datax$logPopBio)
  k_start <- max(datax$logPopBio)
  mint <- datax$Time[datax$PopBio == min(datax$PopBio)]
  maxt <- datax$Time[datax$PopBio == max(datax$PopBio)]
  lm_growth <- lm(logPopBio ~ Time, data = datax[datax$Time > mint & datax$Time < maxt,])
  t_lag_start <- datax$Time[which.max(diff(diff(datax$logPopBio)))] 
  r_max_start <- lm_growth$coefficients[2]
  startingval[,1] <- abs(N_0_start)
  startingval[,2] <- abs(k_start)
  startingval[,3] <- rnorm(100, mean = r_max_start, sd = 2)
  startingval[,4] <- runif(100, min = 0, max = t_lag_start)
  return(startingval)
  
}

gompertz_model <- function(t, r_max, k, N_0,t_lag ){
  return(N_0 + (k - N_0) * exp(-exp(r_max) * exp(1) * (t_lag - t)/ ((k - N_0 * log(10)) + 1)))
}

gompertzbf <- function(x) {
  datax <- data[data$No_id == x & data$PopBio > 0,]
  datax$logPopBio <- log(datax$PopBio)
  strtval <- startvcalc2(x) #change the name of the new starting value function
  endingval <- matrix(NA,100,7)
  
  for (i in 1:nrow(strtval)){
    result <- try({
      fit_gompertz <- nlsLM(logPopBio ~ gompertz_model(t = Time, r_max, k, N_0,t_lag), 
                            data = datax,
                            start = list(r_max = strtval[i, 3], N_0 = strtval[i, 1], k  = strtval[i, 2], t_lag = strtval[i, 4]),
                            control = nls.lm.control(maxiter = 10000),trace = F)
      fit_gompertz
    }, silent = TRUE)
    if (!inherits(result, "try-error")) {
      print(result)
      endingval[i,5] <- AICc_fun(result)
      endingval[i,6] <- RSq_fun(result,datax)
      endingval[i,1] <- coef(result)["r_max"]
      endingval[i,2] <- coef(result)["N_0"]
      endingval[i,3] <- coef(result)["k"]
      endingval[i,4] <- coef(result)["t_lag"]
      endingval[i,7] <- datax$No_id[1] 
    } 
    min_aic_row <- as.vector(endingval[which.min(endingval[,5]),]) # AIC or min r^2
    
    
  }
  return(min_aic_row)
}




######################


gompertzmod2<- function(x) {
  bf <- data.frame(matrix(ncol = 7, nrow = 0))
  
  unique_ids <- unique(x$No_id)
  
  for (j in unique_ids) {
    result <- tryCatch({
      bf <- rbind(bf,gompertzbf(j))
    },error = function(e) {
      warning(paste("Error for No_id", j, "- skipping. Error message:", e$message))
      NULL
    })
    
  }
  colnames(bf) <- c("R_max", "N_0", "k","t_lag", "AIC", "RSq","ID")
  return(bf)
}


results <- gompertzmod2(data)

cubicbf <- function(x) {
  
  unique_ids <- unique(x$No_id)
  endingval <- matrix(NA,length(unique_ids),3)
  for (j in unique_ids) {
    datax <- x[x$No_id == j & data$PopBio > 0,]
    result <- lm(log(PopBio) ~ poly(Time, 3), data = datax)
    endingval[j,1] <- AICc_fun(result)
    endingval[j,2] <- RSq_fun(result,datax)
    endingval[j,3] <- datax$No_id[1] 
  } 
  colnames(endingval) <- c("AICc","RSq","ID")
  return(endingval)
}
results3 <- cubicbf(data)
results3


allplot<- function(dat,coeff,coeff2) {
  
  unique_ids <- unique(coeff$ID)
  for (j in unique_ids) {
    datax <- dat[dat$No_id == j & dat$PopBio > 0,]
    datax$logPopBio <- log(datax$PopBio)
    timepoints <- seq(0, max(datax$Time), 0.1)
    coeffit <- coeff[coeff$ID == j,]
    coeffit2 <- coeff2[coeff2$ID == j,]
    gompertz_points <- gompertz_model(t = timepoints, 
                                      r_max = coeffit[,"R_max"], 
                                      k = coeffit[,"k"], 
                                      N_0 = coeffit[,"N_0"],
                                      t_lag = coeffit[,"t_lag"])
    logistic_points <-  logistic_model(t = timepoints, 
                                       r_max = coeffit2[,"R_max"], 
                                       k = coeffit2[,"k"], 
                                       N_0 = coeffit2[,"N_0"])
    cubicm <- lm(datax$logPopBio ~ poly(datax$Time, 3, raw = T), data = datax)
    cubtime <- seq(max(datax$Time), 0, length.out = length(datax$Time))
    cubicpoints <- predict(cubicm, newdata = data.frame(cubtime))
    df3 <- data.frame(cubtime, cubicpoints)
    df3$model <- "Cubic equation"
    names(df3) <- c("Time", "N", "model")
    df2 <- data.frame(timepoints, logistic_points)
    df2$model <- "Logistic equation"
    names(df2) <- c("Time", "N", "model")
    df1 <- data.frame(timepoints, gompertz_points)
    df1$model <- "gompertz equation"
    names(df1) <- c("Time", "N", "model")
    allmod <- rbind(df1,df2,df3)
    p <- ggplot(datax, aes(x = Time, y = logPopBio)) +
      geom_point() +
      geom_line(data = allmod, aes(x = Time, y = N, col = model),linewidth = 1) +
      theme(aspect.ratio=1)+ # make the plot square 
      labs(x = "Time", y = " Log Cell number") + ggtitle(j)
    print(p) 

  }

}

plots <- allplot(data,results,results1)
plots
ggsave("../Results/Plots.pdf",
       device = "pdf")


outputs1 <- list(data.frame(results3),data.frame(results),data.frame(results1))
outputs <-reduce(outputs1,full_join, by = 'ID')


aiccomp <- cbind(outputs$ID,outputs$AICc,outputs$AIC.x,outputs$AIC.y)
colnames(aiccomp) <- c("ID","Cubic","Gompertz","Logistic")
aiccomp


# Function to count occurrences of each AIC being the lowest
count_lowest_AIC <- function(x) {
  listofwin <- matrix(NA,1,ncol(x))
  for (i in 1:nrow(x)){
    if (!is.na(x[i,2]) & !is.na(x[i,3]) & !is.na(x[i,4])) {
      min_index <- which.min(x[i,-1])
      out <- (names(x[i,-1])[min_index])
      listofwin[i] <- out 
    } else {
      listofwin[i] <- NA 
    } 
    
  }
  return(listofwin)
}



g <- as.factor(count_lowest_AIC(aiccomp))
summary(g)
print(g)

allplotx<- function(dat,coeff,coeff2,x) {
  
  datax <- dat[dat$No_id == x & dat$PopBio > 0,]
  datax$logPopBio <- log(datax$PopBio)
  timepoints <- seq(0, max(datax$Time), 0.1)
  coeffit <- coeff[coeff$ID == x,]
  coeffit2 <- coeff2[coeff2$ID == x,]
  gompertz_points <- gompertz_model(t = timepoints, 
                                    r_max = coeffit[,"R_max"], 
                                    k = coeffit[,"k"], 
                                    N_0 = coeffit[,"N_0"],
                                    t_lag = coeffit[,"t_lag"])
  logistic_points <-  logistic_model(t = timepoints, 
                                     r_max = coeffit2[,"R_max"], 
                                     k = coeffit2[,"k"], 
                                     N_0 = coeffit2[,"N_0"])
  cubicm <- lm(datax$logPopBio ~ poly(datax$Time, 3, raw = T), data = datax)
  cubtime <- seq(max(datax$Time), 0, length.out = length(datax$Time))
  cubicpoints <- predict(cubicm, newdata = data.frame(cubtime))
  df3 <- data.frame(cubtime, cubicpoints)
  df3$model <- "Cubic equation"
  names(df3) <- c("Time", "N", "model")
  df2 <- data.frame(timepoints, logistic_points)
  df2$model <- "Logistic equation"
  names(df2) <- c("Time", "N", "model")
  df1 <- data.frame(timepoints, gompertz_points)
  df1$model <- "gompertz equation"
  names(df1) <- c("Time", "N", "model")
  allmod <- rbind(df1,df2,df3)
  p <- ggplot(datax, aes(x = Time, y = logPopBio)) +
    geom_point() +
    geom_line(data = allmod, aes(x = Time, y = N, col = model),linewidth = 1) +
    labs(x = "Time", y = " Log Cell number") + ggtitle(x) + theme_bw(base_size = 20) +
    theme(aspect.ratio = 1)
  print(p)
  
}

par(mfcol=c(2,1))




plot165 <- allplotx(data,results,results1,165)
ggsave("../Results/fig1a.pdf")
plot112 <- allplotx(data,results,results1,112)
ggsave("../Results/fig1b.pdf")
plot84 <- allplotx(data,results,results1,84)
ggsave("../Results/fig2.pdf")

allplotxmin<- function(dat,coeff,coeff2,x) {
  
  datax <- dat[dat$No_id == x & dat$PopBio > 0,]
  datax$logPopBio <- log(datax$PopBio)
  timepoints <- seq(0, max(datax$Time), 0.1)
  coeffit <- coeff[coeff$ID == x,]
  coeffit2 <- coeff2[coeff2$ID == x,]
  gompertz_points <- gompertz_model(t = timepoints, 
                                    r_max = coeffit[,"R_max"], 
                                    k = coeffit[,"k"], 
                                    N_0 = coeffit[,"N_0"],
                                    t_lag = coeffit[,"t_lag"])
  logistic_points <-  logistic_model(t = timepoints, 
                                     r_max = coeffit2[,"R_max"], 
                                     k = coeffit2[,"k"], 
                                     N_0 = coeffit2[,"N_0"])
  df2 <- data.frame(timepoints, logistic_points)
  df2$model <- "Logistic equation"
  names(df2) <- c("Time", "N", "model")
  df1 <- data.frame(timepoints, gompertz_points)
  df1$model <- "gompertz equation"
  names(df1) <- c("Time", "N", "model")
  allmod <- rbind(df1,df2)
  p <- ggplot(datax, aes(x = Time, y = logPopBio)) +
    geom_point() +
    geom_line(data = allmod, aes(x = Time, y = N, col = model),linewidth = 1) +
    labs(x = "Time", y = " Log Cell number") + ggtitle(x) + theme_bw(base_size = 20) +
    theme(aspect.ratio = 1)
  print(p)
  
}

plot110 <- allplotxmin(data,results,results1,110)
ggsave("../Results/fig3.pdf")
