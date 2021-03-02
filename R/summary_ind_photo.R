# the function returns a tibble by species by site and so on...
summary_ind_photo <- function(dataframe,groupvars){
  #groupvars is a vector contain several variables
  require(tidyverse)
  df <- dataframe %>%
    group_by_at(vars(one_of(groupvars))) %>%
    summarize(n_ind_photos=n())
  df
}
