# Pr. 1, simulace posouzeni, jak funguje vyberovy prumer 
# a vyberove smerodatna odchylky jako odhad stredni hodnoty
# mi a smerodatne odchylky z normalniho rozdeleni
# Budeme studovat pripad, kdy mi=1 a sigma=2, tj. N(1,2)
# Rozsah vyberu zvolime jako n=100

set.seed(123)
# mi, sigma: stredni hodnota a smer. odchylka teoretickeho rozdeleni
mi=1
sigma=2
# n: rozsah vyberu
n=100
# nrep: pocet provedenych nahodnych vyberu
nrep=1000 
# PrumVyb: vektor na ukladani vyberovych prumeru
PrumVyb=rep(0,nrep)
PrumVyb
# PrumSD: vektor na ukladani odmocniny vyb. rozptylu
PrumSD=rep(0,nrep)
PrumSD

for (i in 1:nrep) {
y=rnorm(n,mi,sigma)
PrumVyb[i]=mean(y)
PrumSD[i]=sd(y)
}

summary(PrumVyb)
summary(PrumSD)
hist(PrumVyb)
hist(PrumVyb,freq=F,breaks=seq(0,2,by=0.02))
abline(v=mi,col="red")
# teoreticke rozd. vyb pruměru je opet normalni rozdeleni se stejnou stredni hodnotou mi a smerodatnou odchylkou sigma/sqrt(n)
xtr=seq(0,2,by=0.01)
ytr=dnorm(xtr,mi,sigma/sqrt(n))
lines(xtr,ytr,col="blue",lwd=4)

boxplot(PrumVyb)
abline(h=mi,col="red")

# Vyhodnoceni nestrannosti a variability vyberoveho prumeru
# Nestrannost se posuzuje pomoci vychyleni (bias)
# Odhad vychyleni
Bias_mi=mean(PrumVyb)-mi
Bias_mi
# Variabilita se posoudi pomoci rozptylu nasimulovanych vyberovych prumeru
Var_mi=var(PrumVyb)
Var_mi
# celkova variabilita se posoudi pomoci stredni ctvercove chyby (MSE)
MSE_mi=Var_mi+Bias_mi**2
MSE_mi
# alternativni vypocet MSE
MSE_mi1=mean((PrumVyb-mi)**2)
MSE_mi1
# Rozdil plyne z toho, ze var(PrumVyb) ma v jmenovateli nrep-1 misto poctu replikaci nrep

# vyhodnoceni nestrannosti a variability vyberove smerodatne odchylky
# nestrannost se posuzuje pomoci vychyleni (bias)
# odhad vychyleni
Bias_sigma=mean(PrumSD)-sigma
Bias_sigma
# variabilita se posoudi pomoci rozptylu vyberovych smerodatnych odchylek
Var_sigma=var(PrumSD)
Var_sigma
# celkova variabilita se posoudi pomoci stredni ctvercove chyby (MSE)
MSE_sigma=Var_sigma+Bias_sigma**2
MSE_sigma
# alternativni vypocet MSE
MSE_sigma1=mean((PrumSD-sigma)**2)
MSE_sigma1
# Rozdil plyne z toho, ze var(PrumSD) ma v jmenovateli nrep-1 misto poctu replikaci nrep


# Srovnani prosteho nahodneho vyberu s vracenim a bez vraceni
# V teamu N=7 pracovniku byly vyplaceny pololetni bonusy (v tis. Kc): 4,4,7,10,11,14,40.
bonusy=c(4,4,7,10,11,14,40)
bonusy
# Vybira se n=2,3 pracovniku a na zaklade jejich udaju se odhadnou prumerne bonusy
# Lze vybrat n ruznych pracovniku (vyber bez vraceni) nebo n krat vybereme se stejnou prav. 1 pracovnika (1 pracovnik muze byt vybran vickrat).
# Budou oba postupy davat nestranne odhady?
# Ktery postup byste zvolili v praxi?
# Pro mala N a n lze spocist exaktni rozdeleni (pro vsechny kombinace nebo variace s opakovanim se spocte vyberovy prumer a spocte rel. cetnost)
# Pro prakticka N a n nelze, proto se pouzivaji simulace.
# Navrh simulace

# n: rozsah vyberu
n=2
# nrep: pocet provedenych nahodnych vyberu
nrep=10000 
# PrumVyb: vektor na vysledky vyb. prumeru
# PrumVybSop: vyberovy prumer pro vyber s opakovanim
# PrumVybBop: vyberovy prumer pro vyber bez opakovani (vybira se n ruznych pracovniku)
PrumVybSop=rep(0,nrep)
PrumVybBop=rep(0,nrep)

for (i in 1:nrep) {

PrumVybSop[i]=mean(sample(bonusy,n,replace=TRUE))
PrumVybBop[i]=mean(sample(bonusy,n,replace=FALSE))

}
# Popisne statistiky
# odhadovany prumer
mean(bonusy)
summary(PrumVybSop)
summary(PrumVybBop)
# srovnani variability
var(PrumVybSop)
var(PrumVybBop)
# graficke srovnani
boxplot(PrumVybBop,PrumVybSop,names=c("s vracenim","bez vraceni")
# cervenou carou se vyznaci odhadovany populacni prumer
abline(h=mean(bonusy),col="red")

# Srovani rozdeleni pomoci empiricke distribucni funkce
par(mfrow=c(1,2))
plot(ecdf(PrumVybBop),xlim=c(0,40))
plot(ecdf(PrumVybSop),xlim=c(0,40))

# Uvazujte alternativni strategii, vime, ze vedouci tymu miva extremni premie.
# Nasimulujute alternativni odhad vybery o rozsahu n=2,3, kdy se vzdy vybere pracovnik s pravdepodobnosti 1 a ze zbylych 8 pracovniku se vybere 1, pripadne 2
# Jak byste odhadodli prumernou vysi bonusu?

# Teoreticke rozdeleni vyberoveho prumeru
# Soubor z 1. prednasky, hrube mzdy 9 pracovniku
mzdy=c(12,14,24,25,27,50,57,90,110)
# Popisne statistiky, rozlozeni mezd
summary(mzdy)
var(mzdy)
sd(mzdy)
# Kvantily - vypocet zalozeny pres empirickou districni funkci
quantile(mzdy,probs=seq(0,1,by=0.10),type=4)
quantile(mzdy,probs=seq(0,1,by=0.05),type=4)
# Vypocet populacnich charakteristik
# Populacni prumer
PopMean=mean(mzdy)
PopMean
# Populacni uhrn
PopSum=sum(mzdy)
PopSum
# Populacni roztyl
PopVar=var(mzdy)
# Velikost populace N
N=length(mzdy)
N
# Jak vypocitat populacni rozptyl s velikosti populace N v jmenovateli
# Funkce var() pocita vyberovy rozptyl, tj. jmenovatel je (N-1)
var(mzdy)*(length(mzdy)-1)/length(mzdy)
# Rozpyl je vyrazne mensi
var(mzdy)
# Podil rozptylu mezd a vyberoveho rozptylu
(length(mzdy)-1)/length(mzdy)
# Pro velke populace (N=1000 a vice) je to lze zanedbat
# Par grafu
# Boxplot
graphics.off()
par(mar=c(1,1,1,1))
boxplot(mzdy)
abline(h=PopMean,col="blue")


# Histogram
# Absolutni cetnosti
graphics.off()
 par(mar=c(1,1,1,1))
hist(mzdy)
graphics.off()
 par(mar=c(1,1,1,1))
hist(mzdy,breaks=c(10,20,50,100,110),xlim=c(0,120),ylim=c(0,5),freq=T)
abline(v=PopMean,col="blue")

# Odhad hustoty mezd, nejsou to relativni cetnosti jednotlivych intervalu
hist(mzdy,breaks=c(10,20,50,100,110),xlim=c(0,120),freq=F)

# Teoreticke rozdeleni vyberoveho prumeru 
# Jen prosty nahodny vyber bez vraceni
# Urcime velikost populace N
N=length(mzdy)
# Rozsah vyberu zvolime 3
n=3
# Vygenerujeme individualni mzdy vsech trojic ruznych pracovniku
combn(mzdy,n)
# Spocteme vyberove prumery mezd techto trojic ruznych pracovniku
combn(mzdy,n,FUN=mean)
# Kolik je tech trojic?
choose(N,n)
length(combn(mzdy,n,FUN=mean))
# Distribucni funkce vyberoveho prumeru
graphics.off()

 par(mar=c(1,1,1,1))
plot(ecdf(combn(mzdy,n,FUN=mean)),xlim=c(10,90),main="Distribucni funkce vyberoveho prumeru",ylab="Distr. funkce F",xlab="Mzda (tis. Kc)")

# Pravdepodobnosti funkce P pro vyberovy prumer
# Vypocet cetnosti ruznych hodnot vyberovych prumeru
#absolutni cetnosti 
table(combn(mzdy,n,FUN=mean))
# jake ruzne hodnoty vyberovych prumeru dostaneme?
unique(combn(mzdy,n,FUN=mean))
# kolik ruznych hodnot vyberoveho prumeru dostaneme?
length(unique(combn(mzdy,n,FUN=mean)))
#relativni cetnosti (pravdepodobnostni funkce P)
prop.table(table(combn(mzdy,n,FUN=mean)))
table(combn(mzdy,n,FUN=mean))/length(combn(mzdy,n,FUN=mean))

# Standardni grafy pro diskretni nahodne veliciny nefunguji dobre kvuli velkemu poctu hodnot ruznych vyberovych prumeru
graphics.off()
 par(mar=c(1,1,1,1))
barplot(prop.table(table(combn(mzdy,n,FUN=mean))),xlim=c(0,90),ylim=c(0,0.03),width=1,main="Pravdepodobnostni funkce vyberoveho prumeru"
,xlab="Vyb. prumer mzdy (tis. Kc)",ylab="P(x)")
# Trochu lepsi
graphics.off()
 par(mar=c(1,1,1,1))
plot(prop.table(table(combn(mzdy,n,FUN=mean))),ylim=c(0,0.03),xlim=c(0,90),type="h",main="Pravdepodobnostni funkce vyberoveho prumeru"
,xlab="Vyb. prumer mzdy (tis. Kc)",ylab="P(x)")
graphics.off()
 par(mar=c(1,1,1,1))
hist(combn(mzdy,n,FUN=mean),breaks=seq(15,90,by=5),freq=F,main="Odhad hustoty vyberoveho prumeru")
lines(density(combn(mzdy,n,FUN=mean)),col="blue")
# Pro rychly prehled je lepsi histogram
# Modra krivka je jadrovy odhad hustoty, jedna se o neparametricky (nepredpodkladame tvar rozdelen mezd) spojite hustoty
graphics.off()
 par(mar=c(1,1,1,1))
hist(combn(mzdy,n,FUN=mean),breaks=seq(15,90,by=5),freq=F,main="Odhad hustoty vyberoveho prumeru"
,xlab="Vyb. prumer mzdy (tis. Kc)",ylab="f(x)")
lines(density(combn(mzdy,n,FUN=mean)),col="blue")

# Stredni hodnota vyberovych prumeru
mean(combn(mzdy,n,FUN=mean))
# Rozptyl vyberovych prumeru
var(combn(mzdy,n,FUN=mean))*(length(combn(mzdy,n,FUN=mean))-1)/length(combn(mzdy,n,FUN=mean))

## Odhad relativni cetnosti
## Zajima nasm podil pracovniku s mzdou aspon 50 tis. Kc
mzdy50avic=ifelse(mzdy>=50,1,0)
mzdy
mzdy50avic
# populacni relativni cetnost
pi=mean(mzdy50avic)
# rozsah vyberu n=4
n=4
# velikost populace
N=length(mzdy50avic)
N
# jake rozdeleni ma pocet prac. s mzdou aspon 50 tis. Kc, pokud vybirame n=4
# M: pocet pracovniku s mzdou aspon 50 tis. Kc
M=sum(mzdy50avic)
M

# pravdepodobnostni rozdeleni podily pracovniku s mzdou aspon 50 tis. Kc mezi 4 nahodne vybranymi ruznymi pracovniky
graphics.off()
 par("mar")
 par(mar=c(1,1,1,1))
plot((0:min(n,M))/n,dhyper(0:min(n,M),M,N-M,n),xlab="x",ylab="P(p=x)",main="Pravdepodobnostni funkce odhadu rel. cetnosti p",ylim=c(0,0.5),xlim=c(0,1),type="h")
# Distribucni funkce depodobnostni rozdeleni podily pracovniku s mzdou aspon 50 mezi 4 nahodne vybranymi ruznymi pracovniky
graphics.off()
 par("mar")
 par(mar=c(1,1,1,1))
plot((0:min(n,M))/n,phyper(0:min(n,M),M,N-M,n),xlab="x",ylab="P(p<=x)",main="Pravdepodobnostni funkce odhadu rel. cetnosti p",ylim=c(0,1),xlim=c(0,1),type="s")

# Jak byste nasli presne rozdeleni vyberoveho uhrnu (celkove mzdy) pro prosty nahodny vyber bez vraceni o rozsahu n=4?

# Odhad relativni cetnosti
# Exit poll (pruzkum volicu opoustejici volebni mistostnost)
# Volby do senatu 2024, 1. kolo: 20.-21.9.2024, obvod Praha 17
kandidati=c("Cimprichova","Fischer","Tylova","Urban","Hula","Stastny")
hlasy=c(371,15658,3651,1270,562,7664)
# Favoritem voleb je byvaly diplomat a prezidentsky kandidat Pavel Fischer
# Odhadl by exit poll nahodne vybranych sto volicu opoustejicich volebni mistnost, ze vyhraje?
# Vytvorime zakladni soubor
VysledkyVoleb=rep(kandidati,times=hlasy)
# Pocty ziskanych hlasu
table(VysledkyVoleb=rep(kandidati,times=hlasy))
# Podily ziskanych hlasu
prop.table(table(VysledkyVoleb=rep(kandidati,times=hlasy)))
round(prop.table(table(VysledkyVoleb=rep(kandidati,times=hlasy))),3)
# Podil hlasu pro P. Fischera
pi=sum(ifelse(VysledkyVoleb=="Fischer",1,0))/length(VysledkyVoleb)
pi
# Nasimulujeme si 100 000 vyberu o rozsahu n=100
n=100
NRep=100000
#Vektor pro odhady rel. cetnosti p, inicializace
p=rep(0,NRep)
for (i in 1:NRep) {
y=sample(VysledkyVoleb,n,replace=F)
p[i]=mean(ifelse(y=="Fischer",1,0))
}
# Urcime podil pripady,kdy na zaklade bodoveho odhadu chybne urcite, ze P. Fischer nevyhral v 1. kole
round(sum(ifelse(p<0.5,1,0))/NRep,4)
# Popisne statistiky
summary(p)
# Kvantily - vypocet zalozeny pres empirickou districni funkci
quantile(p,probs=seq(0,1,by=0.10),type=4)
quantile(p,probs=seq(0,1,by=0.05),type=4)
# Prezentujte vhodnymi grafy
# Pravdepodobnostni funkce/hustota/distribucni funkce

# Vyberovy prumer
# Data o kalifornskych strednich skolach - api
install.packages("survey")
library("survey")
# zpristupnit soubory
data(api)
# zakladni soubor vsech 6194 skol s aspon 100 studenti
# promenna api00, API (Academic Performance Index v roce 2000)
# Prosty nahodny vyber 100 skol
## posoudit simulaci presnost odhadu prumerneho API u skoly
## posoudit simulaci presnost odhadu podilu s skol s API pod 650 (cilova hodnota stanovena politiky)

# Data o kalifornych skolach jsou v dataframe apipop
summary(apipop)
# Vytvorime si dve populace
# vektor api00 obsahuje hodnoty API v roce 2000 vsech kalifornskych strednich skol
# vektor api00_pod650 obsahuje 0/1 promennou (1 skola ma API mene nez 650) vsech kalifornskych strednich skol

api00=apipop$api00
api00_pod650=ifelse(api00<650,1,0)

# Nastaveni rozsahu vyberu
n=100
# Urceni velikosti populace
N=length(api00)
# Pocet replikaci (nahodnych vyberu)
NRep=1000
# inicializace vektoru na ulozeni vysledku
api00prum=rep(0,NRep)
api00_pod650freq=rep(0,NRep)
# simulaci prumeru i rel. cetnosti udelame v 1 kroku
# Prikaz nize urci, ktere skoly vyberem
s=sample(N,n,replace=F)
# Hodnoty api00 vybranych skol
api00[s]
# Hodnoty api00_pod650 vybranych skol
api00_pod650[s]
# Simulace
for (i in 1:NRep) {
vybrano=sample(N,n,replace=F)
api00prum[i]=mean(api00[vybrano])
api00_pod650freq[i]=mean(api00_pod650[vybrano])
}
# Vyhodnoceni vysledku
mean(api00)
summary(api00prum)
mean(api00_pod650)
summary(api00_pod650freq)








