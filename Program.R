source("Funkcje.R")
#dane wejściowe
dane<- read.csv(file = "dane.csv", header= TRUE)
show(dane)

par(mfrow=c(1,1))
#wykres zależności tygodniowych przychodów od wydatków na radio i telewizje
plot(dane$radio,dane$przychody, xlab ="wydatki na radio i telewizje", ylab= "przychody")
#model regresji linowej
reglinRadio <-lm(przychody ~ radio, dane)
#korytarze ufności.
korytarze_ufnosci(dane$przychody, dane$radio, 0.95)

#wykres zależności tygodniowych przychodów od wydatków na pokazy sklepowe
plot(dane$sklepy, dane$przychody, xlab ="wydatki na pokazy", ylab= "przychody")
reglinSklepy <-lm(przychody ~ sklepy, dane)
korytarze_ufnosci(dane$przychody, dane$sklepy, 0.95)

dane<- dodaj_sume_radia_i_sklepow(dane)
plot(dane$radioSumSklepy, dane$przychody, xlab ="wydatki na radio, telewizje i pokazy", ylab= "przychody")
reglinRadioSumSklepy <- lm(przychody ~ radioSumSklepy, dane)
korytarze_ufnosci(dane$przychody, dane$radioSumSklepy, 0.95)

reglinRadioSklepy<- lm(przychody ~ radio+sklepy, dane)


#oceny
summary(reglinRadio)
summary(reglinSklepy)
summary(reglinRadioSumSklepy)
summary(reglinRadioSklepy)

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


#Test kolmogorowa
#wg książki jak p-value jest wiesze niz poziom istotnosci to nie ma podstaw do odrzucenia hip. zerowej
ks.test(residRadio, "pnorm", mean = mean(residRadio), sd = sd(residRadio))
ks.test(residSklepy, "pnorm", mean = mean(residSklepy), sd = sd(residSklepy))#tu jakis blad wyskakuje
ks.test(residRadioSumSklepy, "pnorm", mean = mean(residRadioSumSklepy), sd = sd(residRadioSumSklepy))
ks.test(residRadioSklepy, "pnorm", mean = mean(residRadioSklepy), sd = sd(residRadioSklepy))

#test kolmogorowa
#wyznaczamy wart krytyczna
test_res_radio<-kolmogorow_test2(residRadio)
test_res_sklepy<-kolmogorow_test2(residSklepy)
test_res_RadioSumSklepy<-kolmogorow_test2(residRadioSumSklepy)
test_res_radioSklepy<-kolmogorow_test2(residRadioSklepy)# jesli nie nalezy do przedz. to nie odrz. hip. zerowej
#przedzial trzeba wyznaczyc z tablic kolmogorowa

#test normalności rezydów
shapiro.test(residRadio)
shapiro.test(residSklepy) #tego napewno nie mamy powodów do odrzucenia
shapiro.test(residRadioSumSklepy)
shapiro.test(residRadioSklepy)


