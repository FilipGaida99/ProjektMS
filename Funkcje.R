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

kolmogorow_test<-function(rezydua, poziom_istotnosci){
  srednia <- mean(rezydua)
  n <-length(rezydua)
  czestosc <-c()
  for(i in 1:n){
    czestosc<-c(czestosc, i/n ) #i/n
  }
  
  i_minus_1<-c()
  for(i in 1:n){
    i_minus_1<-c(i_minus_1, ((i-1)/n) ) #i-1/n
  }
  
  wart_dystr_rozkl_norm<-c()# F0(x)
  for(i in 1:n){
    wart_dystr_rozkl_norm<-c(wart_dystr_rozkl_norm, pnorm(rezydua[i], mean=mean(rezydua), sd=sd(rezydua))) # F0(x)
  }
  
  d_plus<-abs(czestosc - wart_dystr_rozkl_norm)
  d_minus<-abs(wart_dystr_rozkl_norm - i_minus_1)
  
  wart<-c(max(d_plus), max(d_minus))
  wart_stat_testowej<-max(wart)
  
  return(wart_stat_testowej)
  
  
}

#zwraca wart statystyki testowej
kolmogorow_test2<-function(rezydua){
  sredniaRadio <- mean(rezydua)
  n <-length(rezydua)
  czestosc <-c()
  i_minus_1<-c()
  wart_dystr_rozkl_norm<-c()
  k=1
  while(k <= n){
    czestosc<-c(czestosc, k/n ) #liczy dobrze
    i_minus_1<-c(i_minus_1, ((k-1)/n) ) #i-1/n liczy dobrze
    wart_dystr_rozkl_norm<-c(wart_dystr_rozkl_norm, pnorm(rezydua[k], mean=mean(rezydua), sd=sd(rezydua)))#to nwm ma liczyc wart dystrybuanty rozkl normalnego
    k=k+1
    
  }
  d_plus<-abs(czestosc - wart_dystr_rozkl_norm)
  d_minus<-abs(wart_dystr_rozkl_norm - i_minus_1)
  
  wart<-c(max(d_plus), max(d_minus))
  wart_stat_testowej<-max(wart)
  
  return(wart_stat_testowej)
  
}
