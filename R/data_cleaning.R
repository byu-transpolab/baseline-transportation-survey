library(tidyverse)
library(magrittr)

################################################################################

#Set paths
datapath <- "data/test_data.csv"
coordspath <- "data/coords_list.csv"
questionspath <- "data/question_names.txt"

#Set parameters
unneededcols <- expr(c(StartDate:Finished, LastName:Language))
othercols <- c("mode", "complex", "city", "hs_modes", "gender", "parent_education")
zoneorder <- c(5, 12, 6, 7, 9, 8, 10, 11, 4, 2) #order of zones in activity cols
firstactcols <- expr(first_activity_5:first_activity_2)
lastactcols <- expr(last_activity_5:last_activity_2)
rankcols <- expr(rank_parking:rank_na)

################################################################################

#read in data and exclude unnecessary columns
data <- read_csv(datapath) %>% 
  `colnames<-`(read_lines(questionspath)) %>% 
  {.[-(1:2),]} %>% #remove the first two rows due to their unhelpfulness
  select(-(!!unneededcols)) %>% 
  relocate(ID)


#remove responses without a mode
data %<>% filter(!(is.na(mode) & is.na(mode_other)))


#copy "other" text to main columns
textcols <- which(colnames(data) %in% othercols)

#collapse "other" columns
for(i in textcols) data %<>% replace(i, coalesce(data[[i]], data[[i+1]]))

#remove "other" columns
data %<>% select(-(textcols+1))


#reformat campus zone data
firstact_all <- data %>% 
  select(!!firstactcols) %>% 
  mutate(across(.fns = ~ replace(., . == "Like", "On")))

lastact_all <- data %>% 
  select(!!lastactcols) %>% 
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
  select(-c(!!firstactcols, !!lastactcols)) %>%
  relocate(first_activity, last_activity, .before = school_year)


#reformat rankings for reasons
rankings <- data %>% 
  select(ID, !!rankcols)

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
data %<>% select(-(!!rankcols))



#create coordinates of living location
coordslist <- read_csv(coordspath)

#determine coords from pixels
coord_longitude <- as.numeric(data$coord_x) + 3
coord_latitude <- as.numeric(data$coord_y) + 3

#join lats/longs based on above and predetermined values (coordslist)
data %<>% 
  mutate(longitude = coord_longitude, latitude = coord_latitude) %>% 
  left_join(coordslist, by = c("complex" = "location")) %>% 
  left_join(coordslist, by = c("city" = "location")) %>% 
  #coalesce all columns
  mutate(longitude = coalesce(longitude, longitude.x, longitude.y),
         latitude = coalesce(latitude, latitude.x, latitude.y)) %>% 
  select(-c(coord_x:coord_y, longitude.x:latitude.y)) %>% 
  relocate(longitude, latitude, .after = city)

data
