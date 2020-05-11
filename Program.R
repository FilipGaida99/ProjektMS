#dane wejściowe
dane<- read.csv(file = "dane.csv", header= TRUE)
show(dane)

#wykres zależności tygodniowych przychodów od wydatków na radio i telewizje
plot(dane$radio,dane$przychody, xlab ="wydatki na radio i telewizje", ylab= "przychody")
#model regresji linowej
reglinRadio <-lm(przychody ~ radio, dane)
#rysowanie linii, nie ma tego w poleceniu
abline(reglinRadio)

#wykres zależności tygodniowych przychodów od wydatków na pokazy sklepowe
plot(dane$sklepy, dane$przychody, xlab ="wydatki na pokazy", ylab= "przychody")
reglinSklepy <-lm(przychody ~ sklepy, dane)
abline(reglinSklepy)


radioSumSklepy = dane$radio + dane$sklepy

plot(radioSumSklepy, dane$przychody, xlab ="wydatki na radio, telewizje i pokazy", ylab= "przychody")
reglinRadioSumSklepy <- lm(przychody ~ radioSumSklepy, dane)

reglinRadioSklepy<- lm(przychody ~ radio+sklepy, dane)
abline(reglinRadioSumSklepy)

#oceny??????
summary(reglinRadio)
summary(reglinSklepy)
summary(reglinRadioSumSklepy)
summary(reglinRadioSklepy)

#TODO: korytarze


par(mfrow=c(2,2))
#diagnostyki
plot(reglinRadio)
plot(reglinSklepy)
plot(reglinRadioSumSklepy)
plot(reglinRadioSklepy)

par(mfrow=c(1,1))
#rezydua
residRadio <- resid(reglinRadio)
residSklepy <- resid(reglinSklepy)
residRadioSumSklepy <- resid(reglinRadioSumSklepy)
residRadioSklepy <- resid(reglinRadioSklepy)

hist(residRadio,
     ylim=c(0,20), 
     xlab = "Wartość rezydów", 
     ylab= "Częstotliwość", 
     main = "Histogram  dla\nreklam")
hist(residSklepy,
     ylim=c(0,20), 
     xlab = "Wartość rezydów", 
     ylab= "Częstotliwość", 
     main = "Histogram rezydów dla\npokazów")
     
hist(residRadioSumSklepy,
     ylim=c(0,20), 
     xlab = "Wartość rezydów", 
     ylab= "Częstotliwość", 
     main = "Histogram rezydów dla\nsumy pokazów i reklam")

hist(residRadioSklepy, 
     ylim=c(0,20), xlab = "Wartość rezydów", 
     ylab= "Częstotliwość", 
     main = "Histogram rezydów dla\npokazów i reklam")

#test normalności rezydów
shapiro.test(residRadio)
shapiro.test(residSklepy) #tego napewno nie mamy powodów do odrzucenia
shapiro.test(residRadioSumSklepy)
shapiro.test(residRadioSklepy)


