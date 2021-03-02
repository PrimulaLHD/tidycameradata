frame2obs.matrix <- function(data, varnames = c("gridID", "date", "species"), type.out = "array") {
  # data should be standardized with full observation history
  require(dplyr)
  gridlist <- data[,c("gridID", "date", "species")] %>%
    split(.$gridID) # per grid
  ngrid <- length(gridlist) # number of grids
  daylist <- list()
  for (i in 1:ngrid) {
    daylist[[i]] <- sort(unique(gridlist[[i]][,"date"]))
    names(daylist)[i] <- names(gridlist)[i]
  }

  if (length(varnames)==3) obs.array <- tapply(rep(1,nrow(data)),data[,varnames[1:3]], sum)
  nsp <- length(dimnames(obs.array)[[3]])
  for (i in 1:nsp) {
    for (j in 1:ngrid) {
      obs.array[names(gridlist)[j],as.character(unlist(daylist[[j]])),i][is.na(obs.array[names(gridlist)[j],as.character(unlist(daylist[[j]])),i])]<-0
    }
  }


  if (type.out=="array") return(obs.array)
  if (type.out=="list") {
    obslist <- list()
    for (i in dimnames(obs.array)[[3]]) obslist[[i]] <- obsarray[,,i]
    return(obslist)
  }
}

