rm(list = ls())
library(dplyr)
library(tidyr)

load("all_data_with_neighbor_count.Rdata")
all_data <- all_data %>% filter(is.na(Dollar) == FALSE)

next_year <- all_data %>% select(X, Y, Status, Treatment, Replicate, Year)
next_year <- next_year %>% mutate(Year = Year - 1) %>% rename(StatusNext = Status)

all_transitions <- inner_join(all_data, next_year)

## This could be done year by year, simply use all_transitions <- all_transitions %>% filter(Year == 14)
all_transitions <- all_transitions %>% group_by(Empty, Dollar, Grass, Status, StatusNext) %>% summarise(ObservedTransitions = n())
all_transitions <- all_transitions %>% spread(StatusNext, ObservedTransitions)
colnames(all_transitions) <- c(colnames(all_transitions)[1:4], "pEmpty", "pDollar", "pGrass")
tot_trans <- rowSums(all_transitions [,5:7], na.rm = TRUE)
all_transitions$pEmpty <- all_transitions$pEmpty / tot_trans
all_transitions$pDollar <- all_transitions$pDollar / tot_trans
all_transitions$pGrass <- all_transitions$pGrass / tot_trans

save(all_transitions, file = "transition_probabilities.Rdata")