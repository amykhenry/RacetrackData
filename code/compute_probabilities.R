rm(list = ls())
library(dplyr)
library(tidyr)

load("all_data_with_neighbor_count.Rdata")
all_data <- all_data %>% filter(is.na(Dollar) == FALSE) #gets rid of rows with fewer than full set of neighbors

next_year <- all_data %>% select(X, Y, Status, Treatment, Replicate, Year) #creates an index to match one year to the next
next_year <- next_year %>% mutate(Year = Year - 1) %>% rename(StatusNext = Status) #renames current status so that we can join them

all_transitions <- inner_join(all_data, next_year) #appends Year 2 StatusNext onto Year1 metadata, so there's both Status and StatusNext

## This could be done year by year, simply use all_transitions <- all_transitions %>% filter(Year == 14)

all_transitions <- all_transitions %>% group_by(Empty, Dollar, Grass, Status, StatusNext) %>% summarise(ObservedTransitions = n())
all_transitions <- all_transitions %>% spread(StatusNext, ObservedTransitions) #These two lines show how many transitions happened with each combination of neighbor states
colnames(all_transitions) <- c(colnames(all_transitions)[1:4], "pEmpty", "pDollar", "pGrass") #Relabels the columns to make clear which states things transitioned TO
tot_trans <- rowSums(all_transitions [,5:7], na.rm = TRUE) #creates a total to divide by to get a probability
all_transitions$pEmpty <- all_transitions$pEmpty / tot_trans #turn number of transitions into probabilities
all_transitions$pDollar <- all_transitions$pDollar / tot_trans
all_transitions$pGrass <- all_transitions$pGrass / tot_trans

save(all_transitions, file = "transition_probabilities.Rdata")
