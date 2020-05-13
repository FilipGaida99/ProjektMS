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

histogram_rezyduow <- function(reglin, nazwa){
  rezydua<- resid(reglin)
  hist(rezydua,
       ylim=c(0,20), 
       xlab = "Wartość rezydów", 
       ylab= "Częstotliwość", 
       main = nazwa)
  return(rezydua)
}

korytarze_ufnosci<- function(przychody, wydatki, poziom){
  reglin<- lm(przychody ~ wydatki)
  nowe.wydatki = seq(0,max(wydatki))
  korytarze <- predict.lm(reglin, newdata = data.frame(wydatki = nowe.wydatki),interval="confidence",
                               level = poziom)
  lines(korytarze[,1], col="black")
  lines(korytarze[,2], col="blue", lty=2)
  lines(korytarze[,3], col="blue", lty=2)
  
  return(korytarze)
}