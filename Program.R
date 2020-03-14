#dane wejściowe
przychody<-c(185,136,308,108,269,243,11,191,277,253,177,193,253,187,284,273,295,223,157,238,167,260,232,272,140,223,230,243,266,280,202,352,274,235,210,175,241,294,271,235,181,132,439,157,207,68,254,304,336,185,260)
radio<-c(59,49,93,81,106,69,48,71,69,50,68,40,82,75,99,57,45,61,30,69,64,84,81,84,80,89,65,60,76,51,43,91,56,54,60,48,66,114,60,56,39,30,68,62,80,58,53,69,84,43,68)
sklepy<-c(35,25,10,27,15,41,19,15,24,29,18,25,8,30,19,28,26,11,19,23,29,22,23,36,22,23,18,8,16,29,20,29,19,20,17,27,19,18,7,25,29,4,33,30,29,16,38,35,33,11,22)

(dane = data.frame(przychody, radio,  sklepy))

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


radioSumSklepy = radio + sklepy

plot(radioSumSklepy, dane$przychody, xlab ="wydatki na radio, telewizje i pokazy", ylab= "przychody")
reglinRadioSumSklepy <- lm(przychody ~ radioSumSklepy, dane)

reglinRadioSklepy<- lm(przychody ~ radio+sklepy, dane)

abline(reglinRadioSumSklepy)
