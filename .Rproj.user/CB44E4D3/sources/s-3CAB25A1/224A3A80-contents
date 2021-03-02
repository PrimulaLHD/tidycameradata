naive_occ <- function(data){
  obs_df <- obs_df(data)
  yf <- frame2obs.matrix(obs_df)

  yf[is.na(yf)]<-0 # needs to be done when using tapply: unobserved combinations always get a zero, even with na.rm=T
  naive_occ <- apply(yf,3,FUN=function(x) sum (rowSums(x)>0)/length(x[,1]))
  naive_occ
}
