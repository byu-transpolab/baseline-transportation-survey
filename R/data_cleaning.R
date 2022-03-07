library(tidyverse)
library(magrittr)

#Set paths
datapath <- "data/test_data.csv"



#read in data and exclude unnecessary columns
data <- read_csv(datapath) %>% 
  `colnames<-`(read_lines("data/question_names.txt")) %>% 
  {.[-(1:2),]} %>% 
  select(-c(StartDate:Finished, ID:Language))

#remove responses without a mode
data %<>% filter(!(is.na(mode) & is.na(mode_other)))

#