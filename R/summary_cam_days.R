# the function returns the sampling effort by sampling unit (camera-days) per year by camera ID
summary_cam_days <- function(data,groupvars) {
  # data should be standardized,groupvars should be included year,cameraID,End_Date,Start_Date etc.
  data_tep <- data[,groupvars]
  data_tep$ndays <- as.numeric(difftime(data_tep[,"End_Date"], data_tep[,"Start_Date"]))
  camera_days <- unique(data_tep)
  return(camera_days)
}
