################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################
library(dplyr)
library(tidyr)

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData <- as.matrix(read.csv("../Data/PoundHillData.csv", header = FALSE))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../Data/PoundHillMetaData.csv", header = TRUE, sep = ";")

############# Inspect the dataset ###############

glimpse(MyData)

############# Transpose ###############

MyData <- t(MyData) # I tried to use pivot_wider but was not successful

############# Replace species absences with zeros ###############

MyData  <- MyData %>% replace(MyData == "", 0)

############# Convert raw matrix to data frame ###############

TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F) #stringsAsFactors = F is important!

colnames(TempData) <- MyData[1,] # assign column names from original data

############# Convert from wide to long format  ###############

tidywrang <- gather(TempData, key = "Species", value = "Count", -Cultivation, -Block, -Plot, -Quadrat)


tidywrang<- tidywrang %>% mutate_at(c("Quadrat","Plot","Block", "Cultivation", "Species"), as.factor) %>% mutate_at("Count", as.integer)

glimpse(tidywrang)
############# Exploring the data (extend the script below)  ###############


