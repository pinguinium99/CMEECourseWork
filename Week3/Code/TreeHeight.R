# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

Treedat <- read.csv("Week3/Data/trees.csv") 

TreeHeight <- function(degrees, distance) {
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  return (height)
}

height <- (TreeHeight(37, 40))
print(paste("Tree height is:", height))

for (i in 1:nrow(Treedat)){
  ang <- Treedat[,3]
  dist <- Treedat[,2]
  Tree.Height.m <- TreeHeight(ang, dist)
  Treedatfin <- cbind(Treedat,Tree.Height.m)
  return(Treedatfin)
}

write.csv(Treedatfin, "Week3/Results/treeHts.csv")





