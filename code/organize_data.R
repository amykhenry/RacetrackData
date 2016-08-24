rm(list = ls()) # remove everything in memory
library(dplyr)
library(tidyr)

all_data <- data.frame() # here we will store all the data

# Read all files and parse them
my_files <- list.files("../", pattern = "csv")
for (my_file in my_files){
  # parse the file name
  # Treatment_ReplicateID_Year.csv
  meta_data <- strsplit(my_file, "_")[[1]]
  Treatment <- meta_data[1]
  Replicate <- meta_data[2]
  Year <- as.integer(strsplit(meta_data[3], "\\.")[[1]][1])
  # read the data
  tmp <- read.csv(paste0("../", my_file))[,-1] # remove first column as it is not interesting
  # set colnames to be the x coordinates, and rownames to be the y coordinates
  colnames(tmp) <- 1:ncol(tmp)
  # build a data.frame
  # use a little trick from tidyr
  tmp$Y <- rownames(tmp)
  tmp <- tmp %>% gather("X", "Status", 1:(ncol(tmp)-1)) %>% select(X, Y, Status)
  tmp$Treatment <- Treatment
  tmp$Replicate <- Replicate
  tmp$Year <- Year
  tmp$X <- as.integer(tmp$X)
  tmp$Y <- as.integer(tmp$Y)
  # append to all_data
  all_data <- rbind(all_data, tmp)
}

# save the data
save(all_data, file = "all_data_tidy.Rdata")
