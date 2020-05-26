#dodaje sumę kolumn z wydatkami do ramki danych.
dodaj_sume_radia_i_sklepow <- function(dane){
  dane$radioSumSklepy = dane$radio + dane$sklepy
  return(dane)
}

#generuje 4 wykresy dla diagnostyki modelu regresji podanego jako parametr.
diagnostyka_regresji <-function(reglin){
  par(mfrow=c(2,2))
  plot(reglin)
  par(mfrow=c(1,1))
}

#tworzy histogram rezyduów. Zwraca wyliczone reszty.
histogram_rezyduow <- function(reglin, nazwa){
  rezydua<- resid(reglin)
  hist(rezydua,
       ylim=c(0,20), 
       xlab = "Wartość rezydów", 
       ylab= "Częstotliwość", 
       main = nazwa)
  return(rezydua)
}

#rysuje na istniejącym graficznym przedstawieniu danych prostą regresji oraz korytarze ufności.
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

#wykonuje test Kołmogorowa-Smirnowa. Zwraca wartość statystyki testowej.
lilliefors_test<-function(rezydua){
  n <- length(rezydua)
  sorted <- sort(rezydua)
  standaryzacja = ( (sorted-mean(sorted))/sd(sorted))  #wartosci zmiennej zestandaryzowanej (X0 - Xsr)/s
  dystryb_emp = pnorm(standaryzacja) #liczymy dystrybuanty dla zmiennej zestandaryzowanej
  k = 1
  wektor_czestosci <-c() #i/n inicjalizacja wektora jak NULL
  wektor_minus1 <-c()    #(i-1)/n
  while(k <= n){
    wektor_czestosci<-c(wektor_czestosci, k/n ) #i/n
    wektor_minus1<-c(wektor_minus1, ((k-1)/n) ) #i-1/n
    k=k+1
  } # stablicowanie (i-1)/n oraz i/n
  
  d_plus <- abs(wektor_czestosci - dystryb_emp)
  d_minus <- abs(dystryb_emp - wektor_minus1)
  
  wartosc_stat_test = max(d_plus, d_minus)
  return(wartosc_stat_test)
  
}

#oblicza przedział krytyczny dla testu.
lilliefors_przedzial<-function(rezydua) {
  n<-length(rezydua)
  przedzial<-0.886/sqrt(n)
  
  return(przedzial)
}

#generuje tekst określający wynik przeprowadzonego testu.
lilliefors_hipoteza<-function(wart_stat, przedzial) {
  if (wart_stat<=1 && wart_stat>przedzial)
    return("Odrzucamy hipotezę H0, przyjmujemy H1")
  else
    return("Nie ma podstaw do odrzucenia hipotezy H0")
}
