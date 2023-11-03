
library(ggplot2)
library(maps)

worldmap <- data(worldMapEnv)

data(worldMapEnv)

mapdat <- map_data('world')
load("../Data/GPDDFiltered.RData")

ggplot() + geom_polygon(data = mapdat, aes(x=long, y = lat, group = group),fill = "black", color = "white") + 
  geom_point(data = gpdd,aes(x=long, y = lat), colour = "orange", size = 3)

# The majority of the data is collected in North America and Europe there are a few data points in Africa and Asia.
# This might lead to biases in the data.