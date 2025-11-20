## 4ST414 
## Vyber skupin (cluster sampling)
## Dvoustupnovy vyber


## Priprava prostredi a nacteni erkovych balicku

## Vycisteni dat
rm(list=ls())
## nainstalovani package survey, sampling, SDAResources
## nacteni knihovny
library(plyr)
library(survey)
library(sampling)
## instalace balicku
install.packages("plyr")
install.packages("survey")
install.packages("sampling")

## Vse se bude ilustrovat na souboru svycarskych municipalit z balicku sampling
data(swissmunicipalities)
summary(swissmunicipalities)
attach(swissmunicipalities)
## Promenne pouzite
## REG - region
## CT - kanton
## Nom - jmeno
## H00P01 - pocet jednoclennych domacnosti
## Vytvorit data frame Pop s pouzitymi promennymi
Pop=data.frame(REG=as.character(REG),
CT=as.character(CT),
Nom=Nom,
H00P01=H00P01)

summary(Pop)
names(Pop)

## Pro dvoustupnovy vyber budeme uvazovat jako PSU kanton a SSU obec
## Bude vyber skupin (vyberu kanton a v nem vsechny obce) vhodny?
table(CT)
tapply(H00P01,CT,mean)
tapply(H00P01,CT,sd)
summary(aov(H00P01~as.character(CT)))
## Pocet PSU - kantony
N=length(unique(CT))
N
## Pocet SSU - obce
M=length(Nom)
M
## Pridame fpc
## fpc1 - pocet kantonu
Pop$fpc1=N
## fpc2 - pocet obci v kantonech Mi
popsize_recode=c("1"=171,"2"=400,"3"=107,"4"=20,"5"=30,"6"=7,"7"=11,"8"=29,"9"=11,"10"=242,
"11"=126,"12"=3,"13"=86,"14"=34,"15"=20,"16"=6,"17"=90,"18"=212,"19"=232,"20"=80,
"21"=245,"22"=384,"23"=160,"24"=62,"25"=45,"26"=83)
Pop$fpc2=popsize_recode[Pop$CT]
# Vyber skupin: vyber 3 kantony
## pomoci funkce cluster
set.seed(123)
c1=cluster(Pop,clustername=c("CT"),size=3,method="srswor")
c1
## Vyber (select) dat za obce s vybranych kantonu
vybc1=getdata(Pop,c1)
dim(vybc1)
## vypocet vah a fpc (M - velikost populace PSU)
vybc1$vaha=1/vybc1$Prob
vybc1$fpc=M
## Urceni desingu
sc1=svydesign(id=~CT,weights=~vaha,fpc=~fpc,data=vybc1)


## Odhad poctu jednoclennych domacnosti
totalc1=svytotal(~H00P01,design=sc1)
totalc1
## srovnani
sum(H00P01)
## 95% interval spolehlivosti pro uhrn
confint(totalc1,level=0.95)


## Dvoustupnovy vyber - vybereme pulku kantonu a v kazdem kantonu 3 obce
set.seed(123)
s2ds=mstage(Pop,stage=list("cluster",""),varnames=list("CT"),size=list(13,rep(3,13)),method=c("srswor","srswor"))
Vyber2=getdata(Pop,s2ds)[[2]]
## Survey design
sd2s=svydesign(id=~CT+Nom,fpc=~fpc1+fpc2,data=Vyber2)
totsd2s=svytotal(~H00P01,design=sd2s)
totsd2s
confint(totsd2s)
sum(Vyber2$H00P01/Vyber2$Prob)
## co se stane, kdyz navysime rozsah vyberu v kazdemu kantonu na 5, tj.. mi=5?

mstage(Pop,stage=list("cluster",""),varnames=list("CT"),size=list(13,rep(5,13)),method=c("srswor","srswor"))


