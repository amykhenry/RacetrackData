rm(list = ls())
library(nnet)
rm(list = ls())
library(dplyr)
library(tidyr)

load("all_data_with_neighbor_count.Rdata")
all_data <- all_data %>% filter(is.na(Dollar) == FALSE) #Get rid of rows with fewer than 8 neighbors

next_year <- all_data %>% select(X, Y, Status, Treatment, Replicate, Year) #just select the following years data
next_year <- next_year %>% mutate(Year = Year - 1) %>% rename(StatusNext = Status)#rename the status column

all_transitions <- inner_join(all_data, next_year) append using the id columns the next years data to it. 

# Here one could subset the data (for example filter(Year == 14) would take only the transitions observed 2014->2015)

model_current_status <- multinom(StatusNext ~ as.factor(Status) + Empty + Dollar + Grass, data = all_transitions)#Multinomial logistic model
stupid_model <- multinom(StatusNext ~ 1, data = all_transitions)#null model
exp(as.numeric(logLik(stupid_model)) / nrow(all_transitions)) #basic structure for testing what proportion of the observations it guesses correctly
model_only_neigh <- multinom(StatusNext ~ Empty + Dollar + Grass, data = all_transitions)#mulitnom model of just neighbors
model_only_sign <- multinom(StatusNext ~ as.factor(Status) + I((Dollar) / (Grass + Dollar)), data = all_transitions) #current best fit based on proportions of neighbors

all_transitions$model_st <- rep(0, nrow(all_transitions)) # this script makes an empty column for the probilities to go
all_transitions$model_ne <- rep(0, nrow(all_transitions))
for(i in 1:nrow(all_transitions)){
  my_next <- as.integer(all_transitions$StatusNext[i]) 
  all_transitions$model_st[i] <- model_current_status$fitted.values[i, my_next + 1]
  all_transitions$model_ne[i] <- model_only_neigh$fitted.values[i, my_next + 1]
} #this function puts fitted values of those probabilities into those columns

# all possible states
for_simulations_curr_status <- expand.grid(0:2, 0:8, 0:8, 0:8) #make a structure for all possible combinations of starting conditions, neighbor states
# filter those that don't have exactly 8 neighbors
for_simulations_curr_status <- for_simulations_curr_status[rowSums(for_simulations_curr_status[,-1]) == 8, ] #winnow it down
# make into a data frame
for_simulations_curr_status <- data.frame(for_simulations_curr_status)
colnames(for_simulations_curr_status) <- c("Status", "Empty", "Dollar", "Grass")
tmp <- predict(model_current_status, for_simulations_curr_status, "probs")
colnames(tmp) <- c("pEmpty", "pDollar", "pGrass")
for_simulations_curr_status <- cbind(for_simulations_curr_status, tmp)
# # Example from https://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/
# n <- 1000
# df1 <- data.frame(x1=runif(n,0,100),
#                   x2=runif(n,0,100))
# df1 <- transform(df1,
#                  y=1+ifelse(100 - x1 - x2 + rnorm(n,sd=10) < 0, 0,
#                             ifelse(100 - 2*x2 + rnorm(n,sd=10) < 0, 1, 2)),
#                  set="Original")
# 
# mod <- multinom(y ~ x1 + x2, df1)
# get_probability <- function(x){
#   predict(mod, x, "probs")
# }
