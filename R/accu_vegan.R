accu_vegan <- function(data, method = "random",...){
  require(dplyr)
  #1) get the cols for calculate
  df <- data[,c("cameraID","species","date_time","End_Date","Start_Date")] %>%
    mutate(date=as.Date(date_time)) %>%
    select(cameraID,date,species,End_Date,Start_Date)
  df2 <- df %>%
    select(cameraID,End_Date,Start_Date)
  datef <- unique(df2)
  #2) get full observation history dataframe with empty species
  datelist <- datef %>%
    split(.$cameraID) # by per camera
  ls <- list()
  for (i in 1:length(datelist)) {
    # camera i
    #i=1
    camera=names(datelist)[i]
    tep <- datelist[[i]]
    df_tep <- list()
    for (j in 1:length(tep[,1])) {
      # survey j
      #j=1
      ndays <- as.Date(tep[j,"End_Date"])-as.Date(tep[j,"Start_Date"])+1

      df_tep[[j]] <- data.frame(date=seq(from=as.Date(tep[j,"Start_Date"]),
                                         to=as.Date(tep[j,"End_Date"]),by=1),
                                cameraID=rep(camera,ndays),
                                Start_Date=rep(tep[j,"Start_Date"],ndays),
                                End_Date = rep(tep[j,"End_Date"],ndays))
    }
    ls[[i]] <- do.call("rbind",df_tep)

  }
  obs_df_empty <- do.call("rbind",ls)
  #3) merge obs_df_empty and observed dataframe by cameraID
  obs_df <- dplyr::full_join(df,obs_df_empty,by=c("cameraID","date","Start_Date","End_Date"))
  #4) generate a new col for combining camera and date
  obs_df <- obs_df[!is.na(obs_df$date),] # remove the camera with the ID of species was NA
  obs_df[is.na(obs_df)] <- 0
  obs_df <- obs_df %>% mutate(cameraday=paste(obs_df[,"cameraID"],obs_df[,"date"],
                                              sep = "_"))

  # 5)
  mat <- table(obs_df[,"cameraday"],obs_df[,"species"])
  attributes(mat)$class <- "matrix"

  ac <- specaccum(mat, method = method, ...)
  res <- data.frame(ac$sites, ac$richness, ac$sd)
  colnames(res) <- c("Camera.trap.days", "species", "sd")
  return(res)
}

