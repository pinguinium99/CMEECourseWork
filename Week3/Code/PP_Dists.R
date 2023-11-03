MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv")

head(MyDF)


pdf("../Results/Pred_Subplots.pdf") 
par(mfcol=c(3,2))
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  feed <- subset(MyDF, Type.of.feeding.interaction == i)
  hist(log(feed$Predator.mass), 
       xlab="Body Mass (g)", ylab="Count", main = unique(feed$Type.of.feeding.interaction)) 
}
graphics.off();

pdf("../Results/Prey_Subplots.pdf")
par(mfcol=c(3,2))
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  feed <- subset(MyDF, Type.of.feeding.interaction == i)
  hist(log(feed$Prey.mass), 
       xlab="Body Mass (g)", ylab="Count", main = unique(feed$Type.of.feeding.interaction)) 
}
graphics.off();

MyDF <- mutate(MyDF, Ratio = MyDF$Prey.mass/MyDF$Predator.mass)

pdf("../Results/SizeRatio_Subplots.pdf")
par(mfcol=c(3,2))
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  feed <- subset(MyDF, Type.of.feeding.interaction == i)
  hist(log(feed$Ratio), 
       xlab = "Log Body Mass Ratio", ylab="Count", main = unique(feed$Type.of.feeding.interaction)) 
}
graphics.off();


dat <- setNames(data.frame(matrix(ncol = 3, nrow = 2)), c("log Predator Mass", "log Prey Mass", " log Prey/Predator body mass ratio"))
row.names(dat) <- c("Mean","Median")

dat[1,] <- MyDF %>%  summarise(mean(log(MyDF$Predator.mass)),mean(log(MyDF$Prey.mass)), mean(log(MyDF$Ratio)))
dat[2,] <- MyDF %>%  summarise(median(log(MyDF$Predator.mass)),median(log(MyDF$Prey.mass)), median(log(MyDF$Ratio)))

write.csv(dat, "../Results/PP_Results.csv")
