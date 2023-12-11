#Datuak kargatu proiektuan
Notak = read.table("D:/Descargas/EUSKERA/Notak.txt", header = T)
Notak

#Zutabeak erabiltzeko komandoa
attach(Notak)

#Begiratu lista bakoitzean dauden datuak EzZenbaki kantitatea kontuan artu gabe
length(Notak1P[!is.na(Notak1P)])
length(Notak2P[!is.na(Notak2P)])

#Limiteak jarri
heniaP1=max(Notak1P,na.rm=T)-min(Notak1P, na.rm = T)
heniaP1
heniaP2=max(Notak2P,na.rm=T)-min(Notak2P,na.rm=T)
heniaP2

sqrt(length(Notak1P))#biak berdiank dira

heniaP1/7
heniaP2/7

limiteakP1 = seq(0,10,heniaP1/7)
limiteakP1
limiteakP2 = seq(0,10,heniaP2/7)
limiteakP2

#Banatu tarteka
Notak1P.tarte = cut(Notak1P,limiteakP1, right = F,include.lowest = T)
Notak1P.tarte

#Banatu tarteka bigarren notak
Notak2P.tarte = cut(Notak2P,limiteakP2, right = F,include.lowest = T)
Notak2P.tarte

#Sortu tablak datuekin
table(Notak1P.tarte)
table(Notak2P.tarte)

#kalkulu guztiak Notak1P datuekin
a = as.data.frame(table(Notak1P.tarte))
a
Tarteak = a$Notak1P.tarte
length(Tarteak)
Maiz.abs = a$Freq
Maiz.abs
sum(Maiz.abs)
Maiz.erl = Maiz.abs/sum(Maiz.abs)
Maiz.erl
length(Maiz.erl)
Maiz.abs.met = cumsum(Maiz.abs)
Maiz.abs.met
length(Maiz.abs.met)
Maiz.erl.met = cumsum(Maiz.erl)
Maiz.erl.met
length(Maiz.erl.met)
data.frame(Tarteak,Maiz.abs,Maiz.erl,Maiz.abs.met,Maiz.erl.met)

barplot(table(Notak1P.tarte), col = "red")

pie(table(Notak1P.tarte))

hist(Notak1P, main = "Noten histograma", xlab = "Notak", ylab = "Dentsitatea", col = "red")

stem(Notak1P, scale=2)

#kalkulu guztiak Notak2P datuekin
b = as.data.frame(table(Notak2P.tarte))
b
Tarteak.b = b$Notak2P.tarte
length(Tarteak.b)
Maiz.abs.b = b$Freq
Maiz.abs.b
sum(Maiz.abs.b)
Maiz.erl.b = Maiz.abs.b/sum(Maiz.abs.b)
Maiz.erl.b
length(Maiz.erl.b)
Maiz.abs.met.b = cumsum(Maiz.abs.b)
Maiz.abs.met.b
length(Maiz.abs.met.b)
Maiz.erl.met.b = cumsum(Maiz.erl.b)
Maiz.erl.met.b
length(Maiz.erl.met.b)
data.frame(Tarteak.b,Maiz.abs.b,Maiz.erl.b,Maiz.abs.met.b,Maiz.erl.met.b)

barplot(table(Notak2P.tarte), col = "blue")

pie(table(Notak2P.tarte))

hist(Notak2P, main = "Noten histograma", xlab = "Notak", ylab = "Dentsitatea", col = "blue")

stem(Notak2P, scale=2)

#Batezbestekoa

bb = mean(Notak1P, na.rm = T)
bb

#BatezbestekoaP2
bb2 = mean(Notak2P, na.rm = T)
bb2

#Mediana

me=median(Notak1P, na.rm = T)
me

#MedianaP2
me2=median(Notak2P, na.rm = T)
me2

#Moda

table(Notak1P)

moda = names(table(Notak1P))[which(table(Notak1P)==max(table(Notak1P)))]
moda

#ModaP2
table(Notak2P)

moda2 = names(table(Notak2P))[which(table(Notak2P)==max(table(Notak2P)))]
moda2

#Heinak (berriro)

max = max(Notak1P, na.rm = T)
min = min(Notak1P, na.rm = T)

Heina = max(max)-min(min)
Heina

max2 = max(Notak2P, na.rm = T)
min2 = min(Notak2P, na.rm = T)

Heina2 = max(max2)-min(min2)
Heina2

#Kuartilarteko heina

IQR(Notak1P, type=2, na.rm = T)

#Kuartilarteko heinaP2
IQR(Notak2P, type=2, na.rm = T)

#Bariantza

var = var(Notak1P, na.rm = T)

N = length(Notak1P)
N

bar = var*(N-1)/N
bar
#BariantzaP2
var2 = var(Notak2P, na.rm = T)

N2 = length(Notak2P)
N2

bar2 = var2*(N2-1)/N2
bar2

#Desbiderazio tipikoa

des.tip = sqrt(bar)
des.tip

#Desbiderazio tipikoaP2

des.tip2 = sqrt(bar2)
des.tip2

#Aldakuntza koefizientea

CV = des.tip/bb
CV

#Aldakuntza koefizienteaP2

CV2 = des.tip2/bb2
CV2

#Kuartilak

quantile(Notak1P,type=2,na.rm = T)

quantile(Notak2P,type=2,na.rm = T)

#Kuantilak

quantile(Notak1P,probs=seq(0,1,0.1),type=2,na.rm = T)

quantile(Notak2P,probs=seq(0,1,0.1),type=2,na.rm = T)

#Fisher alborapen koefizientea, moments paketea instalatu eta gero
library(moments)
skewness(Notak1P, na.rm = T)

skewness(Notak2P, na.rm = T)

#Pearson alborapen koefizientea

Pearson = (bb-1)/des.tip
Pearson

#Pearson alborapen koefizienteaP2
Pearson2 = (bb2-1)/des.tip2
Pearson2

#Kurtosia

k = kurtosis(Notak1P, na.rm = T)-3
k

#KurtosiaP2
k2 = kurtosis(Notak2P, na.rm = T)-3
k2

#Kutxa diagrama eta balio arraroak

boxplot(Notak1P, horizontal=T, col="red",type=2)

boxplot.stats(Notak1P)

boxplot(Notak2P, horizontal=T, col="blue",type=2)

boxplot.stats(Notak2P)

#Batezbesteakoaren estimazio puntuala

mean(Notak1P, na.rm = T)

#Batezbesteakoaren estimazio puntualaP2

mean(Notak2P, na.rm = T)

#Bariantzaren estimazio puntuala

var(Notak1P, na.rm = T)

#Bariantzaren estimazio puntualaP2
var(Notak2P, na.rm = T)

#Desbiderazio tipikoaren estimazio puntuala

sd(Notak1P, na.rm = T)

#Desbiderazio tipikoaren estimazio puntualaP2

sd(Notak2P, na.rm = T)

###########################################

