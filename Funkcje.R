#dodaje sumę kolumn z wydatkami do ramki danych.
dodaj_sume_radia_i_sklepow <- function(dane){
  dane$radioSumSklepy = dane$radio + dane$sklepy
  return(dane)
}

diagnostyka_regresji <-function(reglin){
  par(mfrow=c(2,2))
  plot(reglin)
  par(mfrow=c(1,1))
}