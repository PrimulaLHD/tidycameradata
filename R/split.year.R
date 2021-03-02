## if your survey period was crossing years, but you want to Split by year------
split.year <- function(data){
  # data should be a cameradata object!
  require(lubridate)
  require(tidyverse)
  n1 <- length(data[,"Start_Date"])
  n2 <- length(data[,"End_Date"])
  if(n1 != n2) stop("the number of rows of start date unequal to that of end date!")

  strart <- vector(length = n1)
  end <- vector(length = n2)
  for (i in 1:n1) {
    if (data[i,"Year"] == year(data[i,"Start_Date"])) strart[i]=data[i,"Start_Date"]
    if (data[i,"Year"] > year(data[i,"Start_Date"])) strart[i]= as.character(make_date(data[i,"Year"],1,1))
  }
  for (j in 1:n2) {
    if (data[j,"Year"] == year(data[j,"End_Date"])) end[j]=data[j,"End_Date"]
    if (data[j,"Year"] < year(data[j,"End_Date"])) end[j]= as.character(make_date(data[j,"Year"],12,31))
  }
  data[,"Start_Date"] <- strart
  data[,"End_Date"] <- end
  return(data)
}



