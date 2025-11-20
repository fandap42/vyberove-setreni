## Balanced sample
## Vycisteni dat
rm(list=ls())
## nainstalovani package sampling
## instalace balicku
install.packages("sampling")
## nacteni knihovny
library(sampling)
## nainstalovani package survey
## instalace balicku
install.packages("survey")
## nacteni knihovny
library(survey)

## datove soubory
## 2296 svycarskych municipalit
data(swissmunicipalities)
names(swissmunicipalities)
help(swissmunicipalities)
summary(swissmunicipalities)

## 284 svedskych municipalit
data(MU284)
names(MU284)
help(MU284)
summary(MU284)

## 589 belgickych municipalit
data(belgianmunicipalities)
names(belgianmunicipalities)
help(belgianmunicipalities)
summary(belgianmunicipalities)

# Uzitecne funkce pro strafitikaci
str1=c(-2,3,-2,3,4,4,4,-2,-2,3,4,0,0,0)
str2=rep(c("modra","cervena","bila"),times=c(4,8,5))
# cleanstrata: prevede funkci na cisla strata h=1,2,3,..,L
# textove promenne usporada abecedne
cleanstrata(str1)
cleanstrata(str2)

# disjunctive: pro vyber z L strat vytvori 0-1 matice, 1 v danem sloupci, pokud tam jednotka patri
disjunctive(str1)
disjunctive(str2)

## cube method ... jednoducha implementace
## samplecube
### X: matice pomocnych promennych, podle ktery se bilancuje
### pik: pravdepodobnostni zahrnuti i-te jednotky do vyberu
### order: 1=data nejsou usporadana, 3 data jsou usporadana dle y
### comment: TRUE: vypis algoritmu a statistiky vybalancovanosti
### method: landing phase, jak resit necelociselne vybery:
### 1 pomoci linearniho programovani, 2 pomoci potlaceni promenne


## Vyber 50 svedskych municipalit
## A) prosty nahodny vyber bez vraceni
N=length(MU284$P75)
N
pikA=rep(50/N,times=N)
pikA
## B) vyber proporcinalni poctu obyv. v roce 1975 (P75)
pikB=inclusionprobabilities(MU284$P75,50)
pikB

## Vyvazeny vyber vzhledem k poctu obyvatel v roce 1975 a poctu mist v municipalni rade
## v roce 82, tj. P75 a S82
set.seed(31)
## Zde nekontrolujeme rozsah vyberu
sA1=samplecube(MU284$S82,pikA,order=1,comment=TRUE,method=1)
## odhad uhranu P85 pocet obyvatel a RMT85: danove prijmy municipality
## pocet vybranych jednotek
sum(sA1)
## odhad uhrnu RMT85
HTestimator(y=MU284$RMT85[sA1==1],pik=pikA[sA1==1])
## populacni uhrn
sum(MU284$RMT85)
## odhad rozptylu Devilleho metodou
varest(MU284$RMT85[sA1==1],pik=pikA[sA1==1])
## odhadnete priblizny interval spolehlivosti pro uhrn

## obdobne bychom udelali, kdybychom chteli kontroloval rozsah vyberu
samplecube(cbind(MU284$S82,rep(1,N)),pikA,order=1,comment=TRUE,method=1)
## Odhadnete uhrn danovych prijmu RMT85


## proporcionalni vyber
## budeme kontroval i dodrzeni celkovaho rozsahu vyberu
set.seed(145)
sB1=samplecube(cbind(MU284$S82,rep(1,N)),pikB,order=1,comment=TRUE,method=1)
## rozsah vyberu
sum(sB1)
## 
## odhad uhrnu RMT85
HTestimator(y=MU284$RMT85[sB1==1],pik=pikB[sB1==1])
## populacni uhrn
sum(MU284$RMT85)
## odhad rozptylu Devilleho metodou
varest(MU284$RMT85[sB1==1],pik=pikB[sB1==1])
## urcete priblizny 95% interval spolehlivosti pro uhrn

## Domnivate se, ze vyse uvedeny odhad se vylepsi, pokud budeme bilancovat i pomoci regionalni promenne REG?

## provedeni stratifikovaneho vyberu
## nahodny vyber 50 municipalting, bilancovani na pocet clenu rady S82
## bilancovat pomoci poctu
## a) priblizne pomoci samplecube 
## vygenerujeme matici X pomocnych promennych
## pro strata se prida osm sloupcu nula-jednickovych promennych
disjunctive(MU284$REG)
## vytvoreni matice X (pomocne promenne pro bilancovani vyberu)
X=cbind(MU284$S82,rep(1,N),disjunctive(MU284$REG))
X
sA2=samplecube(X,pikA,order=1,comment=TRUE,method=1)
## stratifikovany (proporcionalni vyber se podarilo zajistit pouze priblizne)
## odhad uhrnu RMT85
HTestimator(y=MU284$RMT85[sA2==1],pik=pikA[sA2==1])
## populacni uhrn
sum(MU284$RMT85)
## odhad rozptylu Devilleho metodou
varest(MU284$RMT85[sA2==1],pik=pikA[sA2==1])
## odhadnete priblizny interval spolehlivosti pro uhrn

## Provedte pro proporcionalni vyber


## Stratifikovany vyber
## usporadat dataframe MU284 podle stratifikacni promenne 
MU284o=MU284[order(MU284$REG),]
MU284o
## A) proporcionalni alokace do strat
pikA3=rep(50/N,times=N)
sA3=balancedstratification(cbind(MU284o$S82,rep(1,N)),MU284o$REG,pikA3,comment=TRUE)
## vyber je vyvazeny
## odhad uhrnu RMT85
HTestimator(y=MU284o$RMT85[sA3==1],pik=pikA3[sA3==1])
## populacni uhrn
sum(MU284o$RMT85)
## odhad rozptylu Devilleho metodou
varest(MU284$RMT85[sA3==1],pik=pikA3[sA3==1])

## Pouzijeme funkce pro stratifikovany vyber z balicku survey

## vybereme data pro vybrane zamestnace
sample_strat=MU284o[sA3==1,]
sample_strat
## pomoci pravdepodobnosti vyberu k-te jednotky se spoctou vahy.
sample_strat$vaha=1/pikA3[sA3==1]
sample_strat$vaha

## kontrola suma vah ve stratu se musi rovnat velikosti populace Nh ve stratu
tapply(sample_strat$vaha,sample_strat$REG,sum)
table(MU284$REG)

## jeste se do vyberu musi pridat sloupec pro fpc = velikost populace ve stratu Nh
popsize_recode=c("1"=25,"2"=48,"3"=32,"4"=38,"5"=56,"6"=41,"7"=15,"8"=29)
sample_strat$fpc=popsize_recode[as.character(sample_strat$REG)]
sample_strat
# kontrola promenne fpc
sample_strat$fpc

## vytvori se survey design
sd_unif=svydesign(id=~1,strata=~as.character(REG),weights=~vaha,fpc=~fpc,data=sample_strat)

## odhad uhrnu danovych prijmu za celou populaci
svytotal(~RMT85,sd_unif)



## odhady rozpylu jsou pouze orientacni

## intervaly spolehlivosti pro populacni uhrn

confint(svytotal(~RMT85,sd_unif),level=0.95)



## odhady uhrnu v jednotlivych stratech
svyby(~RMT85,by=~as.character(REG),sd_unif,svytotal,keep.var=TRUE)

## interval spolehlivosti
confint(svyby(~RMT85,by=~as.character(REG),sd_unif,svytotal,keep.var=TRUE))

## chceme vytahnout odhady prumeru ve stratech
coef(svyby(~mzdy,by=~vzdel,sd_unif,svymean,keep.var=TRUE))
## chceme vytahnout sm. odchylku (SE) ve stratech
SE(svyby(~mzdy,by=~vzdel,sd_unif,svymean,keep.var=TRUE))


## Zlepsi kvalitu odhadu pokud budeme bilancovat i dle poctu obyvatel z drivejska? (P75)






