source("Funkcje.R")
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


dane<- dodaj_sume_radia_i_sklepow(dane)
plot(dane$radioSumSklepy, dane$przychody, xlab ="wydatki na radio, telewizje i pokazy", ylab= "przychody")
reglinRadioSumSklepy <- lm(przychody ~ radioSumSklepy, dane)
abline(dane$reglinRadioSumSklepy)

reglinRadioSklepy<- lm(przychody ~ radio+sklepy, dane)


#oceny
summary(reglinRadio)
summary(reglinSklepy)
summary(reglinRadioSumSklepy)
summary(reglinRadioSklepy)

#TODO: korytarze
new.radio = seq(min(dane$radio), max(dane$radio), by=1.0)
korytarze_ufnosci <- predict(reglinRadio, newdata = data.frame(radio = new.radio), interval="confidence",
                         level = 0.95)
lines(korytarze_ufnosci[,], col="blue", lty=2)
lines(korytarze_ufnosci[,3], col="blue", lty=2)

#diagnostyki
diagnostyka_regresji(reglinRadio)
diagnostyka_regresji(reglinSklepy)
diagnostyka_regresji(reglinRadioSumSklepy)
diagnostyka_regresji(reglinRadioSklepy)

#rezydua
residRadio <- histogram_rezyduow(reglinRadio,"Histogram  dla\nreklam")
residSklepy <- histogram_rezyduow(reglinSklepy, "Histogram rezydów dla\npokazów")
residRadioSumSklepy <- histogram_rezyduow(reglinRadioSumSklepy, "Histogram rezydów dla\nsumy pokazów i reklam")
residRadioSklepy <- histogram_rezyduow(reglinRadioSklepy, "Histogram rezydów dla\npokazów i reklam")


#test normalności rezydów
shapiro.test(residRadio)
shapiro.test(residSklepy) #tego napewno nie mamy powodów do odrzucenia
shapiro.test(residRadioSumSklepy)
shapiro.test(residRadioSklepy)


