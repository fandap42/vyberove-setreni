## Cv. 3: Regresni a pomerovy odhad
## Vycisteni dat
rm(list=ls())
## nainstalovani package survey, sampling, SDAResources
## instalace balicku
install.packages("SDAResources")
install.packages("survey")
install.packages("sampling")
## nacteni knihovny
library(SDAResources)
library(survey)
library(sampling)


## MU284 - soubor o 284 svedskych municipalitach
## Analyzovane promenne:
## RMT85: danovy prijem municipality v roce 1985 (mil. SEK)
## P75: populace municipality v roce 1975, tis. obyv.
## P85: populace municipality v roce 1985, tis. obyv.
data(MU284)
## Zpristupnime jednotlive promenne z dataframe MU284
attach(MU284)

## Vypoctete prumerny danovy prijem na obyvatele v roce 1985

## Takto ne, je potreba mit vazeny prumer
RMT85/P85
mean(RMT85/P85)

## Je to podil celkovych danovych vynosu k poctu obyvatel v roce 1985
sum(RMT85)
sum(P85)
## Tak to ma byt spravne.
sum(RMT85)/sum(P85)


## Vztah mezi poctem obyv. v roce 1985 a danovymi prijmy
plot(P85,RMT85)
cor(P85,RMT85)
## Jaky je vliv velkych municipalit?
## Zpristupnime jednotlive promenne z dataframe MU284
attach(MU284)

## Vztah mezi poctem obyv. v roce 1975 a 1985
plot(P75,P85)
cor(P75,P85)

## Budeme odhadovat uhrny a prumery u municipalit, ktere v roce 1975 mely mene nez 200 tis. obyv

which(P75<200)

## K nazvum promennych pridame m jako male municipality

P75m=P75[which(P75<200)]
P85m=P85[which(P75<200)]
RMT85m=RMT85[which(P75<200)]

## Zobrazime si data a jak funguje prima umera
plot(P75m,P85m)
abline(0,mean(P85m)/mean(P75m),col="blue")
cor(P75m,P85m)

## Zobrazime si data a jak funguje prima umera
plot(P75m,RMT85m)
abline(0,mean(RMT85m)/mean(P75m),col="blue")
cor(P75m,RMT85m)

## Urceni velikosti populace a populacnich uhrnu

N=length(P75m)
N
tP75m=sum(P75m)
tP85m=sum(P85m)
tRMT85m=sum(RMT85m)
tP75m
tP85m
tRMT85m
cor(P75m,P85m)
cor(P85m,RMT85m)

## linearni regrese
## odhad primky y=a+bx
fit1<-lm(RMT85~P85)
summary(fit1)
names(fit1)
# # # Jak získat hodnoty regresních populačních koeficientů A,B
A<-summary(fit1)$coefficients[1]
B<-summary(fit1)$coefficients[2]
A
B

plot(P85,RMT85)
abline(coef=coef(fit1),col="red")

## odhad primky y=bx (a=0, tzv. umerova regrese)
fit2<-lm(RMT85~P85-1)
summary(fit2)
names(fit2)
# # # Jak získat hodnoty regresních populačních koeficientů A,B
B1<-summary(fit2)$coefficients[1]
B1
coef(fit2)
abline(coef=c(0,coef(fit2)),col="blue")

## Provedte 10% vyber a odhadnete pomoci vyberoveho prumeru, pomeroveho a regresnich odhadu
## (umerova regrese i regrese s absolutnim clenem) celkove danove prijmy v roce 1985
## u malych municipalit, jako pomocna promenna X je pocet obyv. v roce 1985
## Pak pak temito metodami odhadnete prumerny danovy vynos male municipality v roce 1985

## Urceni rozsahu vyberu
N=length(P85m)
n=round(0.1*N)
n
## Provedeme vyber
s=sample(N,n,replace=F)
s
## Udaje o vybranych municipalitach
P85ms=P85m[s]
P75ms=P75m[s]
RMT85ms=RMT85m[s]

## Dataframe pro vyber
vyberm=data.frame(P75ms,P85ms,RMT85ms)
summary(vyberm)
## Doplnime si weight (vah) a popsize (N)
vyberm$weight=N/n
vyberm$popsize=N

## Vytvori se objekt pro survey design - prosty nah. vyber
sd1=svydesign(id=~1,weights=~weight,fpc=~popsize,data=vyberm)

## Odhad uhrnu RMT85 pomoci vyberoveho uhrnu
VybUhrn=svytotal(~RMT85ms,design=sd1)
VybUhrn
confint(VybUhrn)

## Odhad uhrnu RMT85 pomoci pomeroveho odhadu (P85 je pomocna promena)
VybPomer=svyratio(numerator=~RMT85ms,denominator=~P85ms,design=sd1)
VybPomer
confint(VybPomer)
## skutecny pomer RMT85 a P85
tRMT85m/tP85m
sum(RMT85m)/sum(P85m)
## odhad populacniho uhrnu jako predikce s vyuzitim vyberoveho pomeru pro x=uhrn P85m
predict(VybPomer,total=sum(P85m))
## intervaly spolehlivosti rucne s pomoci total a se
predict(VybPomer,total=sum(P85m))$total
predict(VybPomer,total=sum(P85m))$se

## Regresni odhady
## Model 1: y=a+bx
model1=svyglm(RMT85ms~P85ms,design=sd1)
model1
confint(model1)
## odhad uhrnu
## dataframe pro hodnotu pomocne promenne P85
newdata=data.frame(P85ms=sum(P85m))
## Odhad uhrnu, do promene total dat rozsah populace
predict(model1,newdata,total=N)
## Intervaly spol. opet pomoci funkce confint
confint(predict(model1,newdata,total=N))
## Model 2: y=bx
model2=svyglm(RMT85ms~P85ms-1,design=sd1)
model2
confint(model2)
## odhad uhrnu
## dataframe pro hodnotu pomocne promenne P85
newdata=data.frame(P85ms=sum(P85m))
## Odhad uhrnu, do promene total dat rozsah populace
predict(model2,newdata,total=N)
## Intervaly spol. opet pomoci funkce confint
confint(predict(model2,newdata,total=N))

## Proc se odhad pomoci 2. modelu (y=bx) a pomeroveho odhadu nerovna?

# urceni rozsahu vyberu pro pozadovanou presnost pro oboustranny interval spolehlivosti
# Pouze pro rel. cetnost:
# delta: pozadovana presnost (pulka sirka intervalu, presnost plus minus delta)
# kvantil_z: 1-alfa/2 kvantil normovaneho normalniho rozdeleni
# sigma2: rozptyl odhadovane promenne
# N: velikost populace
# corr: korelace mezi Y a pomocnou promennou X

# S vyuzitim vysledku vyberu pro rok 1985 urcete minimalni rozsah vyberu pro rok 1986, pokud chceme odhadnout celkove RMT s presnosti 1 mld. SEK pri spolehlivosti 95 %
delta=1000/N
## pozadovanou presnost delime N, protoze pouzijeme vzorec pro rozsah prumeru.
alfa=0.05
spol=1-alfa/2
# proc pouzivame normovane normalni rozdeleni a ne t-rozdeleni
kvantil_z=qnorm(spol)
kvantil_z
# pro sigma2 vyuzijeme vyberovy rozptyl RMT85m
sigma2=var(RMT85ms)
# pro corr vyuzijeme vyberovy korelacni koeficient
corr=cor(RMT85ms,P85ms)
corr
# Plati priblizny vztah, ze rozpyl se upravi na sigma2*(1-corr^2)
n=1/(delta*delta/(kvantil_z*kvantil_z*sigma2*(1-corr*corr))+1/N)
# jak byste zaoukrohlili vypocteny rozsah vyberu?
# nahoru, kdyby vyslo n=24.6, tak vyber n=24 neda pozadovanou minimalni presnost delta
n=ceiling(1/(delta*delta/(kvantil_z*kvantil_z*sigma2*(1-corr*corr))+1/N))
n


## Aplikujte na odhad poctu obyvatel malych municipality (mene nez 200 tis. obyv. v roce 1975) na odhad obyvatel techto municipalit v roce 1985
## Pomerove, rozdilove odhady byly studovany v souvislosti odhadu predbeznych vysledku scitani lidu v 50. letech v USA a Japonsku.
## Velka mesta se odhadla expertne, pripadne se zpracovala vsechna data, za male obce se provedl vyber.
## Uvazujete 10, 20 % vyber pro odhad obyvatel v mensich municipalitach (zajima to politiky, geografy, obyvatele malych mest i velkych).
## Srovnejte jednotlive metody. Kterou preferujete a proc?





