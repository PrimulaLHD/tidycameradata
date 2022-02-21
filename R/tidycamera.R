tidycamera <- function(data,gridID,cameraID,year,species,date_time,End_Date,Start_Date,interval = 30){
  require(tidyverse)
  # data should be a dataframe or a tibble
  # cameraID,year,species, and date_time are the columns you must set up
  # cameraID,year,species, and date_time cannot set up to NULLS
  # typically, the threshold of interval between two independent photos is 30 mins.
  
  datar <- data
  
  if (!any(names(datar)=="year"))  datar <- datar %>% mutate(year = datar[,year])
  if (!any(names(datar)=="gridID"))  datar <- datar %>% mutate(gridID = datar[,gridID])
  if (!any(names(datar)=="cameraID"))  datar <- datar %>% mutate(cameraID = datar[,cameraID])
  if (!any(names(datar)=="species"))  datar <- datar %>% mutate(species = datar[,species])
  if (!any(names(datar)=="date_time"))  datar <- datar %>% mutate(date_time = datar[,date_time])
  if (!any(names(datar)=="End_Date"))  datar <- datar %>% mutate(End_Date = datar[,End_Date])
  if (!any(names(datar)=="Start_Date"))  datar <- datar %>% mutate(Start_Date = datar[,Start_Date])
  
  datar1 <- datar %>%
    filter(!is.na(date_time)) # to avoid no photops in a given grid and a camera
  
  #datar1 <- droplevels(datar[which(is.na(datar$date_time)),]) # to avoid no photops in a given grid and a camera
  
  datar2 <- datar1 %>% arrange(date_time) # sort date and time
  data_by_year <- datar2 %>%
    split(.$year) # generate a data list by year
  n_year <- length(data_by_year)
  
  tep1 <- list()
  #i=1
  for (i in 1:n_year) {
    # loop for year
    tep_by_cameraID <- data_by_year[[i]] %>%
      split(.$cameraID) # generate a data list by camera ID
    
    tep2 <- list()
    # j=1
    for (j in 1:length(tep_by_cameraID)) {
      # loop for camera ID
      tep_by_camera_species <-  tep_by_cameraID[[j]] %>%
        split(.$species) # generate a data list by species name
      
      # next calculate the time difference between two photos
      tep_by_camera_species_timediff <- list()
      
      for (k in 1:length(tep_by_camera_species)) {
        # loop for species
        tep <- tep_by_camera_species[[k]]
        if(length(tep[,1]) > 1){
          tep_by_camera_species_timediff[[k]] <- tep %>%
            mutate(timediff = c(1e5,as.numeric(diff(strptime(tep$date_time,"%Y-%m-%d %H:%M:%S")),
                                             units="mins"))) %>%
            mutate(idependent = (timediff > interval))
          # here the first value should be always larger then interval threshold,
          # so I set 1e5
          
        } else{
          tep_by_camera_species_timediff[[k]] <- tep %>%
            mutate(timediff = 1e5) %>%
            mutate(idependent = (timediff > interval))
        }
        
        
      }
      tep2[[j]] <- do.call("rbind",tep_by_camera_species_timediff)
      
    }
    tep1[[i]]<- do.call("rbind",tep2)
    
    
  }
  
  res1 <- do.call("rbind",tep1)
  
  nas_camera_data <- datar %>%
    filter(is.na(date_time)) %>%
    mutate(timediff = NA,
           idependent = TRUE)
  
  res1 <- rbind(res1, nas_camera_data) # keep Nas in your data
  
  out <- res1 %>%
    filter(idependent == TRUE)
  #class(res2) <- "cameradata"
  return(out)
}
