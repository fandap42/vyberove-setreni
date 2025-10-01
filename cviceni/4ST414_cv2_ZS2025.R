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

## Prosty nahodny vyber bez vraceni
## Vycisteni dat
rm(list=ls())

## Pouzite R balicky
## sampling: metody vyberu, datove soubory
install.packages("sampling")
library("sampling")

## Pouzity datovy soubor MU284
## Data o 284 svedskych municipalitach
## 11 promennych z let 1985 a 1975
## Analyzovana promenna RMT85 prijmy rozpoctu municipality z obecnich dani (mil. SEK) v roce 1985
data(MU284)
RMT85=MU284$RMT85
## popisne statistiky, graf
summary(RMT85)
quantile(RMT85,probs=seq(0,1,by=0.05),type=4)
graphics.off()
par(mar=c(1,1,1,1))
boxplot(RMT85)

## Urceni velikosti populace
N=length(RMT85)


## 3 zpusoby vyberu
## standardni funkce sampling
rozs_vyb=20
set.seed(123)
s1=sample(N,rozs_vyb,replace="F")
MU284[s1,]
vyb1=RMT85[s1]
vyb1
## nebo primo
set.seed(123)
sample(RMT85,rozs_vyb,replace="F")

## balicek dplyr
install.packages("dplyr")
library("dplyr")
# vyber 5 municipalit
## Nefunguje pro vyber z vektoru
sample_n(RMT85,size=5,replace="F")
## Musime vybrat z data.frame
sample_n(MU284,size=5,replace="F")
#vyber 10 % municipalit
sample_frac(MU284,size=0.1,replace="F")

## pomoci balicky sampling
## funkce srswor (simple random sampling without replacement)
## vektor pro celou populaci: 1 vybrano, 0 nevybrano
set.seed(123)
s2=srswor(rozs_vyb,N)
## vyber municipalit, ktere byly vybrany
which(s2==1)

vyb2=RMT85[which(s2==1)]

## Odhad prumeru, rozptyl, standarni chyba, dvojstranne intervaly spolehlivosti
## 10% vyber municipalit
## Vycisteni dat
rm(list=ls())
## Analyzovana promenna RMT85 prijmy rozpoctu municipality z obecnich dani (mil. SEK) v roce 1985
data(MU284)
RMT85=MU284$RMT85

## Urceni rozsahu vyberu
N=length(RMT85)

## 10% vyber municipalit
0.1*N
floor(0.1*N)
ceiling(0.1*N)

## urceni 10% rozsahu vyberu (setrime naklady)
n=floor(0.1*N)
n
## provedeme vyber
set.seed(123)

y_RMT85=sample(RMT85,n,replace="F")

## odhad prumerych rozpoctovych prijmu municipality z obecnich dani (mil. SEK) v roce 1985

prum_est=mean(y_RMT85)

## Pop. prumer

prum_RMT85=mean(RMT85)

## Srovnani odhadu s pop. prumerem
prum_est
prum_RMT85

## odhad rozptylu
var_prum_est=(1-n/N)*var(y_RMT85)/n
var_prum_est

## standard error
stderr_prum_est=sqrt(var_prum_est)
stderr_prum_est

## 100(1-alfa) procentni oboustranne intervaly spolehlivosti
## 95% interval spolehlivosti
alfa=0.05
spol=1-alfa/2
## ci_delta , polovina sirky oboustranneho intervalu spolehlivosti
ci_delta=qt(spol,n-1)*sqrt(var_prum_est)
prum_CI=c(prum_est-ci_delta,prum_est+ci_delta)
names(prum_CI)=c("dolni mez","horni mez")
prum_CI




## druhy zpusob: pomoci balicku survey
## instalace a nactani balicku
## survey: metody odhadu a vyberu, datove soubory
install.packages("survey")
library("survey")

## z vyberu je nutne udelat dataframe
## tvori promenne z vyberu
## fpc (finite population correction) velikost populace pro kazdou jednotku z vyberu, pro prosty nahodny vyber vzdy N
## weights (vahy = 1/pravdepodobnost zahrnuti do vyberu, pro prosty nahodny vyber N/n)
vyber=data.frame(y_RMT85,weights=rep(N/n,times=n),fpc=rep(N,times=n))
names(vyber)=c("RMT85","weights","fpc")

vyber
summary(vyber)

## Pro vyber je treba definovat objekt svydesign - tam rikam, jaky jsme pouzili vyber, zda pouzivame pomocne promenne, oblasti (strata) atd.)
sd1=svydesign(id=~1,weights=~weights,fpc=~fpc,data=vyber)

## Bodovy odhad prumeru promenne RMT85 a std. error, promenna a pak nazev objektu svydesing
smean=svymean(~RMT85,sd1)
smean
## shoda s presnymi vzorci

## priblizne oboustranne 95% intervaly spolehlivosti - obecne
confint(smean,level=0.95,df=n-1)
## shoda s presnymi vzorci

## Lze pouzit i pro uhrn, funkce svytotal


## odhadnete prumerne rozpoctove prijmy municipality v roce 1985
## pomoci 10% vyberu
## 1. strategie - vybirejte z cele populace 284 jednotek
## 2. strategie - municipality s extremne velkymi prijem (3 mld. SEK a vice) vybrat s pravdepodobnosti 1, ze zbytku provest prosty nahodny vyber
## 2. strategie: rozdeleni populace MU284 na dve casti
which(RMT85>=3000)
which(RMT85<3000)

RMT85[which(RMT85>=3000)]
RMT85[which(RMT85<3000)]
## Porovnat pomoci simulace (1000 vyberu tyto strategie)

## 2. strategie: rozdeleni populace MU284 na dve casti
## Populace velkych mest a jeji velikost
PVelkeRMT85=RMT85[which(RMT85>=3000)]
NVelke=length(PVelkeRMT85)
NVelke
## Populace ostatnich mest a jeji velikost
POstRMT85=RMT85[which(RMT85<3000)]
NOst=length(POstRMT85)
NOst
# Urceni rozsahu vyberu
# Populaci velkych mest vyberu celou
nVelke=NVelke
nVelke
nOst=floor(0.1*N)-nVelke
nOst

## Odhad prumeru dostaneme jako odhad uhrun (sectou se odhady za velka a ostani mesta)
## Podeli se to celkovym poctem municipalit N=284
## Pocet replikaci
NRep=5000
prum2st=rep(0,NRep)
##
for (i in 1:NRep) {
vybrano=sample(POstRMT85,nOst,replace=F)
prumVel=mean(PVelkeRMT85)
prumOst=mean(vybrano)
prum2st[i]=(NVelke*prumVel+NOst*prumOst)/(N)
}
summary(prum2st)
mean(RMT85)




## Relativni cetnost
## lze vyuzit vzorce pro prumer pro vektor s 0-1 promennymi.
## 2. kolo senatnich voleb 2024 v obvodu Praha 2
## prof. M. Barta ziskal 6352 hlasu
## MUDr. M. Hilser ziskal 5298 hlasu
## Vycisteni dat
rm(list=ls())
## Vytvoreni populace ziskanych hlasu pro exit poll: 1 hlas prof. M. Barta, 0 hlas MUDr. Hilser
kolo2Pop=rep(c(1,0),times=c(6352,5298))
sum(kolo2Pop)
## Populacni podil p, podil hlasu pro prof. Bartu
p=mean(kolo2Pop)
p
## Urceni velikosti populace N
N=length(kolo2Pop)
N
## exit poll 100 volicu
n=100
## provedeme vyber
set.seed(123)

s_poll2kolo=sample(kolo2Pop,n,replace="F")

## odhad ziskanych hlasu pro prof. Bartu
p_est=mean(s_poll2kolo)



## Srovnani odhadu s pop. prumerem
p_est
p

## odhad rozptylu
## proc davaji oba vzorce stejny vysledek?
(1-n/N)*var(s_poll2kolo)/n
(1-n/N)*p_est*(1-p_est)/(n-1)

## vypocet odhadu rozptylu
var_p_est=(1-n/N)*var(s_poll2kolo)/n
var_p_est

## standard error
stderr_p_est=sqrt(var_p_est)
stderr_p_est

## 100(1-alfa) procentni oboustranne intervaly spolehlivosti
## 95% interval spolehlivosti
alfa=0.05
spol=1-alfa/2
## ci_delta , polovina sirky oboustranneho intervalu spolehlivosti
ci_delta=qt(spol,n-1)*sqrt(var_p_est)
prum_CI=c(p_est-ci_delta,p_est+ci_delta)
names(prum_CI)=c("dolni mez","horni mez")
prum_CI

## odhad p pomoci balicku survey
library("survey")
## tvorba dataframe z vyberoveho souboru
vyber2=data.frame(s_poll2kolo,weights=rep(N/n,times=n),fpc=rep(N,times=n))
names(vyber2)=c("proBartu","weights","fpc")
## Pro vyber je treba definovat objekt svydesign - tam rikam, jaky jsme pouzili vyber, zda pouzivame pomocne promenne, oblasti (strata) atd.)
sd2=svydesign(id=~1,weights=~weights,fpc=~fpc,data=vyber2)

## Bodovy odhad prumeru promenne RMT85 a std. error, promenna a pak nazev objektu svydesing
smean2=svymean(~proBartu,sd2)
smean2
## shoda s presnymi vzorci

## priblizne oboustranne 95% intervaly spolehlivosti - obecne
confint(smean2,level=0.95,df=n-1)
## shoda s presnymi vzorci

## Navrhnete simulaci, zda pro n=100 95% intervaly spolehlivosti dodrzuji tuto 95% spolehlivost
## Pocet provedenych vyberu (replikaci nastavte na 5000)
NRep=5000
## CI_p: 1 - populacni podil p se pro dany vyber nachazi v asympot. intervalu spol., 0 jinak
CI_p=rep(0,times=NRep)
## 95% interval spolehlivosti
alfa=0.05
spol=1-alfa/2



## Simulace
set.seed(321)


## Simulace
for (i in 1:NRep) {
vybrano=sample(kolo2Pop,n,replace=F)
p_est=mean(vybrano)
CI_p[i]=ifelse(abs(p_est-p)<qt(spol,n-1)*sqrt((1-n/N)*var(s_poll2kolo)/n),1,0)
}
## Vyhodnoceni vysledku
mean(CI_p)


# urceni rozsahu vyberu pro pozadovanou presnost pro oboustranny interval spolehlivosti
# Pouze pro rel. cetnost:
# delta: pozadovana presnost (pulka sirka intervalu, presnost plus minus delta)
# kvantil_z: 1-alfa/2 kvantil normovaneho normalniho rozdeleni
# sigma2: rozptyl odhadovane promenne
# velikost populace

# Urcete minimalni rozsah vyberu exit pollu, pokud chceme se spolehlivosti 95% odhadnout pop. podil volicu prof. Barty s presnosti 2 proc. body
delta=0.02
alfa=0.05
spol=1-alfa/2
# proc pouzivame normovane normalni rozdeleni a ne t-rozdeleni
kvantil_z=qnorm(spol)
kvantil_z
# pro sigma2 musime mit apriorni info o podilu p
# proc volime hodnotu 0.5 v druhem kole?
p_prior=0.5
sigma2=p_prior*(1-p_prior)
n=1/(delta*delta/(kvantil_z*kvantil_z*sigma2)+1/N)
# jak byste zaoukrohlili vypocteny rozsah vyberu?
# nahoru, kdyby vyslo n=24.6, tak vyber n=24 neda pozadovanou minimalni presnost delta
n=ceiling(1/(delta*delta/(kvantil_z*kvantil_z*sigma2)+1/N))
n
# pomoci simulace posudte, v kolika procentech pripadu spravne odhadneme vitezstvi prof. Barty
# a zda bude dodrzena sirka intervalu spolehlivosti
# pocet replikaci
NRep=1000
# Pro vysledky
ProBartuOK=rep(0,NRep)
SirkaCI=rep(0,NRep)

for (i in 1:NRep) {
vybrano=sample(kolo2Pop,n,replace=F)
p_est=mean(vybrano)
ProBartuOK[i]=ifelse(p_est>0.5,1,0)
SirkaCI[i]=qt(spol,n-1)*sqrt((1-n/N)*var(vybrano)/n)
}
# podil spravne odhadnuteho vitezstvi prof. Barty
mean(ProBartuOK)
# dodrzeni sirky intervalu
mean(ifelse(SirkaCI>delta,0,1))
summary(SirkaCI)



