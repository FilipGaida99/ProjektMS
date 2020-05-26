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

#wykres zależności tygodniowych przychodów od wydatków na reklamę radiową i pokazy sklepowe
dane<- dodaj_sume_radia_i_sklepow(dane)
plot(dane$radioSumSklepy, dane$przychody, xlab ="wydatki na radio, telewizje i pokazy", ylab= "przychody")
reglinRadioSumSklepy <- lm(przychody ~ radioSumSklepy, dane)
korytarze_ufnosci(dane$przychody, dane$radioSumSklepy, 0.95)

#model regresji dla przychodów od wydatków na reklamę radiową i pokazy sklepowe.
reglinRadioSklepy<- lm(przychody ~ (radio+sklepy), dane)

#oceny
show(summary(reglinRadio))
show(summary(reglinSklepy))
show(summary(reglinRadioSumSklepy))
show(summary(reglinRadioSklepy))

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


#test lilieforsa
wart_stat_Radio <- lilliefors_test(residRadio)
wart_stat_Sklepy <- lilliefors_test(residSklepy)
wart_stat_RadioSumSklepy <- lilliefors_test(residRadioSumSklepy)
wart_stat_RadioSklepy <- lilliefors_test(residRadioSklepy)

#przedzial
przedzial_Radio <- lilliefors_przedzial(residRadio)
przedzial_Sklepy <- lilliefors_przedzial(residSklepy)
przedzial_RadioSumSklepy <- lilliefors_przedzial(residRadioSumSklepy)
przedzial_RadioSklepy <- lilliefors_przedzial(residRadioSklepy)

#hipoteza
hipoteza_Radio <- lilliefors_hipoteza(wart_stat_Radio, przedzial_Radio)
hipoteza_Sklepy <- lilliefors_hipoteza(wart_stat_Sklepy, przedzial_Sklepy)
hipoteza_RadioSumSklepy <- lilliefors_hipoteza(wart_stat_RadioSumSklepy, przedzial_RadioSumSklepy)
hipoteza_RadioSklepy <- lilliefors_hipoteza(wart_stat_RadioSklepy, przedzial_RadioSklepy)

#wypisanie wyników testowania hipotez
show(hipoteza_Radio)
show(hipoteza_Sklepy)
show(hipoteza_RadioSumSklepy)
show(hipoteza_RadioSklepy)
