accu_hill <- function(data,by="site",...){
  # data should be standardized
  # by = "site", returns interpolation and extrapolation of species diversity for each site/grid
  # by = "year", returns interpolation and extrapolation of species diversity for each year
  # by = "total", returns interpolation and extrapolation of species diversity for all-year and all-site data
  #
  require(iNEXT)
  if (by=="site"){
    data <- data[!is.na(data$'species'),]
    mat <- table(data[,"species"],data[,"gridID"])
    attributes(mat)$class <- "matrix"
    out <- iNEXT::iNEXT(mat,...)

  } else if (by == "year"){
    mat <- table(data[,"species"],data[,"year"])
    attributes(mat)$class <- "matrix"
    out <- iNEXT::iNEXT(mat,...)
  } else if (by == "total"){
    mat <- table(data[,"species"],data[,"gridID"])
    attributes(mat)$class <- "matrix"
    vec <- rowSums(mat)
    out <- iNEXT::iNEXT(vec,...)
  } else {
    stop("Please choose a valid way for calculation!")
  }
  out
}
