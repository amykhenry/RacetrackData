rm(list = ls())
library(dplyr)
library(tidyr)
# load the data
load("all_data_tidy.Rdata")

# get a subset for testing
#all_data <- all_data %>% filter(Treatment == "ZR", Replicate == "E", Year == 16)

# Let's keep it simple
neigh_shifts <- expand.grid(c(1,0,-1), c(1,0,-1))
neigh_shifts <- neigh_shifts[-5,] # remove 0, 0
count_neigh <- data.frame(Empty = rep(NA, nrow(all_data)),
                          Dollar = rep(NA, nrow(all_data)),
                          Grass = rep(NA, nrow(all_data)))
for (i in 1:nrow(all_data)){
  Treat <- all_data[i,]$Treatment
  Repl <- all_data[i,]$Replicate
  Yr <- all_data[i,]$Year
  my_x <- all_data[i,]$X
  my_y <- all_data[i,]$Y
  my_status <- all_data[i,]$Status
  all_neighbors <- cbind(my_x + neigh_shifts[,1], my_y + neigh_shifts[,2])
  neigh_data <- all_data %>% filter(X %in% all_neighbors[,1],  # The cell is a neighbor of the current cell
                      Y %in% all_neighbors[,2],
                      Treatment == Treat, # same metadata
                      Replicate == Repl,
                      Year == Yr) 
  ## Three extra columns for the count of neighbors
  #print(nrow(neigh_data))
  if (nrow(neigh_data) == 9){
    count_neigh[i,]$Empty <- sum(neigh_data$Status == 0)
    count_neigh[i,]$Dollar <- sum(neigh_data$Status == 1)
    count_neigh[i,]$Grass <- sum(neigh_data$Status == 2)
    # Remove self
    if (my_status == 0) count_neigh[i,]$Empty <- count_neigh[i,]$Empty - 1
    if (my_status == 1) count_neigh[i,]$Dollar <- count_neigh[i,]$Dollar - 1
    if (my_status == 2) count_neigh[i,]$Grass <- count_neigh[i,]$Grass - 1
  }
}
all_data <- cbind(all_data, count_neigh)
save(all_data, file = "all_data_with_neighbor_count.Rdata")
