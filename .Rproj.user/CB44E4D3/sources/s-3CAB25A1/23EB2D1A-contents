# the function returns RAI (i.e., relative abundance index)
RAI_fun <- function(data,bygrid=FALSE,by_year=FALSE,method="RAI_2"){
  # there are four main methods:
  # RAI_1: Carbone et al, 2001
  #RAI_2: Kawanishi et al, 1999
  #RAI_3: Wu et al, 2012
  require(janitor)
  require(tidyverse)
  require(reshape2)
  require(tidyverse)
  # the first step is to calculate the sampling effort

  sam_eff_df <- summary_cam_days(data = data,groupvars = c("gridID","year", "cameraID","End_Date","Start_Date" ))
  tot_eff <- sum(sam_eff_df[,"ndays"]) # total sampling efforts for all years and grids
  tot_eff_grid <- sam_eff_df %>%
    group_by(gridID) %>%
    summarise(ndays = sum(ndays)) # total sampling efforts by grid

  tot_eff_year <- sam_eff_df %>%
    group_by(year) %>%
    summarise(ndays = sum(ndays)) # total sampling efforts by year

  sam_eff_ls <- sam_eff_df %>%
    split(.$year) # generate a data list by year

  n_year <- length(sam_eff_ls) # number of years
  tot_eff_year_ls <- list()

  for( i in 1:n_year){
    tot_eff_year_ls[[i]] <- sam_eff_ls[[i]] %>%
      group_by(gridID) %>%
      summarise(ndays = sum(ndays))
  } # sampling efforts in each grid and each year
  ##--------------------------------------------------------------------------

  if (bygrid){
    if (by_year){
      dat_tep <-  data %>% tabyl(gridID, species, year)
      #debug by HD

      for (i in 1:length(dat_tep)) {
        dat_tep[[i]] <- dat_tep[[i]][dat_tep[[i]]$'gridID' %in% pull(tot_eff_year_ls[[i]][,1]),]
      }


      if (method=="RAI_1"){
        out1 <- list()
        for (i in 1:n_year) {
          tep <- cbind(tot_eff_year_ls[[i]],pull(tot_eff_year_ls[[i]],ndays)/dat_tep[[i]][,-1]) %>%
            mutate(year=names(dat_tep)[i])
          out1[[i]] <- melt(tep,id=c("gridID","year","ndays"))
        }
        out <- do.call("rbind",out1)
        names(out)[4:5] <- c("species","RAI_1")

      } else if (method=="RAI_2"){
        out1 <- list()
        for (i in 1:n_year) {
          tep <- cbind(tot_eff_year_ls[[i]],(dat_tep[[i]][,-1]/pull(tot_eff_year_ls[[i]],ndays))*100) %>%
            mutate(year=names(dat_tep)[i])
          out1[[i]] <- melt(tep,id=c("gridID","year","ndays"))
        }
        out <- do.call("rbind",out1)
        names(out)[4:5] <- c("species","RAI_2")

      } else if (method=="RAI_3"){
        out1 <- list()
        for (i in 1:n_year) {
          tep <- cbind(tot_eff_year_ls[[i]],(dat_tep[[i]][,-1]/rowSums(dat_tep[[i]][,-1]))*100) %>%
            mutate(year=names(dat_tep)[i])
          out1[[i]] <- melt(tep,id=c("gridID","year","ndays"))
        }
        out <- do.call("rbind",out1)
        names(out)[4:5] <- c("species","RAI_3")

      } else {
        stop("Please choose a valid method!")
      }

    }else {
      dat_tep <-  data %>% tabyl(gridID, species)
      if (method=="RAI_1"){
        tep <- cbind(tot_eff_grid,pull(tot_eff_grid,ndays)/dat_tep[,-1])
        out <- melt(tep,id=c("gridID","ndays"))
        names(out)[3:4] <- c("species","RAI_1")

      } else if (method=="RAI_2"){
        #tep <- cbind(tot_eff_grid,(dat_tep[,-1]/tot_eff_grid[,"ndays"])*100) # why not??? HD: due to tbl
        tep <- cbind(tot_eff_grid,(dat_tep[,-1]/pull(tot_eff_grid,ndays))*100)
        out <- melt(tep,id=c("gridID","ndays"))
        names(out)[3:4] <- c("species","RAI_2")

      } else if (method=="RAI_3"){
        tep <- cbind(tot_eff_grid,(dat_tep[,-1]/rowSums(dat_tep[,-1]))*100)
        out <- melt(tep,id=c("gridID","ndays"))
        names(out)[3:4] <- c("species","RAI_3")

      } else {
        stop("Please choose a valid method!")
      }
    }
  }

  if(!bygrid){
    if (by_year){
      dat_tep <-  data %>% tabyl(year, species)
      if (method=="RAI_1"){
        tep <- cbind(tot_eff_year,pull(tot_eff_year,ndays)/dat_tep[,-1])
        out <- melt(tep,id=c("year","ndays"))
        names(out)[3:4] <- c("species","RAI_1")

      } else if (method=="RAI_2"){
        tep <- cbind(tot_eff_year,(dat_tep[,-1]/pull(tot_eff_year,ndays))*100)
        out <- melt(tep,id=c("year","ndays"))
        names(out)[3:4] <- c("species","RAI_2")

      } else if (method=="RAI_3"){
        tep <- cbind(tot_eff_year,(dat_tep[,-1]/rowSums(dat_tep[,-1]))*100)
        out <- melt(tep,id=c("year","ndays"))
        names(out)[3:4] <- c("species","RAI_3")

      } else {
        stop("Please choose a valid method!")
      }
    }else {
      dat_tep <-  data %>% tabyl(gridID, species)
      if (method=="RAI_1"){
        out1 <- tot_eff/colSums(dat_tep[,-1])
        out <- data.frame(cbind(names(out1), out1)) %>%
          mutate(ndays=tot_eff)
        names(out)[1:2] <- c("species","RAI_1")


      } else if (method=="RAI_2"){
        out1 <- (colSums(dat_tep[,-1])/tot_eff)*100
        out <- data.frame(cbind(names(out1), out1)) %>%
          mutate(ndays=tot_eff)
        names(out)[1:2] <- c("species","RAI_2")

      } else if (method=="RAI_3"){
        out1 <- (colSums(dat_tep[,-1])/sum(dat_tep[,-1]))*100
        out <- data.frame(cbind(names(out1), out1)) %>%
          mutate(ndays=tot_eff)
        names(out)[1:2] <- c("species","RAI_3")

      } else {
        stop("Please choose a valid method!")
      }
    }
  }
  return(out)
}
