library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(broom)
MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv")
graphics.off()
p <- ggplot(MyDF, aes(x=Prey.mass, y = Predator.mass, colour=Predator.lifestage)) +
  geom_point(shape = 3) +
  geom_smooth(method='lm',fullrange=TRUE) +
  scale_x_log10(limits=c(1e-08, 1e03),breaks = c(1e-07,1e-03,1e+01)) +
  scale_y_log10(limits=c(1e-7, 1e7),breaks = c(1e-6,1e-2,1e+2,1e+6)) +
  ylab("Predator Mass in Grams")+
  xlab("Prey Mass in Grams")+
  facet_wrap(~Type.of.feeding.interaction, ncol=1, strip.position = "right") + 
  theme_bw() + theme(legend.position = 'bottom')
print(p)

# Max it export a pdf at a given size
# Hard code sizes of text in theme to make it reflect his sizes
# Make sure of all text formatting

ggsave("../Results/PP_Regress.pdf", p)

dat <- MyDF %>%  group_split(Predator.lifestage, Type.of.feeding.interaction) %>% 
  lapply(., function(x) list(feeding = x$Type.of.feeding.interaction[[1]], 
                             lifestage = x$Predator.lifestage[[1]],
                             mod = lm(Predator.mass ~ Prey.mass, data = x)))
dat

datafm <- setNames(data.frame(matrix(ncol = 7, nrow = 18)), c("Predator_life","Feeding_type","regression_slope", "regression_intercept", "R", "F_statistic_value","p_value"))


for (i in 1:length(dat)) {
  val <- summary(dat[[i]]$mod)
  datafm[i,]$Predator_life <- dat[[i]]$lifestage
  datafm[i,]$Feeding_type <- dat[[i]]$feeding
  datafm[i,]$regression_intercept <- dat[[i]]$mod$coef[[1]]
  datafm[i,]$regression_slope <- dat[[i]]$mod$coef[[2]]
  datafm[i,]$R <- val$adj.r.squared
  datafm[i,]$F_statistic_value <- val$fstatistic[1]
  datafm[i,]$p_value <- ifelse(nrow(val$coefficients) > 1, val$coefficients[2,4], NA )
}

write_csv(datafm, "../Results/PP_Regress_Results.csv")
