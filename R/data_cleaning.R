library(tidyverse)
library(magrittr)

#Set paths
datapath <- "data/test_data.csv"
coordspath <- "data/coords_list.csv"

#Set parameters
othercols <- c("mode", "complex", "city", "hs_modes", "gender", "parent_education")
zoneorder <- c(5, 12, 6, 7, 9, 8, 10, 11, 4, 2)


#read in data and exclude unnecessary columns
data <- read_csv(datapath) %>% 
  `colnames<-`(read_lines("data/question_names.txt")) %>% 
  {.[-(1:2),]} %>% #remove the first two rows due to their unhelpfulness
  select(-c(StartDate:Finished, LastName:Language)) %>% 
  relocate(ID)


#remove responses without a mode
data %<>% filter(!(is.na(mode) & is.na(mode_other)))


#copy "other" text to main columns
textcols <- which(colnames(data) %in% othercols)

for(i in textcols){
  col1 <- i
  col2 <- i+1
  for(j in 1:nrow(data)){
    data[j,col1] <- ifelse(is.na(data[j,col1]), data[j,col2], data[j,col1])
  }
}

#remove "other" columns
data %<>% select(-(textcols+1))


#reformat campus zone data
firstact_all <- data %>% 
  select(first_activity_5:first_activity_2) %>% 
  mutate(across(.fns = ~ replace(., . == "Like", "On")))

lastact_all <- data %>% 
  select(last_activity_5:last_activity_2) %>% 
  mutate(across(.fns = ~ replace(., . == "Like", "On")))

zoneorder <- zoneorder

get_locations <- function(df){
  #find which columns are "On"
  cols <- which(df == "On", arr.ind = T) %>%
    as_tibble() %>%
    arrange(row) %>% 
    select(col) %>% 
    unlist() %>% 
    unname()
  
  #use the column index to get the zone
  zoneorder[cols]
}

firstact <- get_locations(firstact_all)
lastact <- get_locations(lastact_all)

#add new activity cols and remove old ones
data %<>% 
  mutate(first_activity = firstact,
         last_activity = lastact) %>% 
  select(-(first_activity_5:last_activity_2)) %>%
  relocate(first_activity, last_activity, .before = school_year)


#reformat rankings for reasons
rankings <- data %>% 
  select(ID, rank_parking:rank_na)

rankings %<>% 
  pivot_longer(-ID) %>% 
  filter(!is.na(value)) %>% 
  arrange(ID,value) %>% 
  pivot_wider(ID, names_from = value, values_from = name) %>% 
  #make the values look prettier
  mutate(across(.fns = ~ gsub("rank_", "", .))) %>% 
  mutate(across(-ID, ~ str_to_title(.)))

#re-add rankings to table
data %<>%
  left_join(rankings, by = "ID") %>% 
  rename(rank_1 = `1`,
         rank_2 = `2`,
         rank_3 = `3`,
         rank_4 = `4`,
         rank_5 = `5`) %>% 
  relocate(rank_1:rank_5, .after = reasons)

#remove old ranking columns
data %<>% select(-(rank_parking:rank_na))



#create coordinates of living location
coordslist <- read_csv(coordspath)