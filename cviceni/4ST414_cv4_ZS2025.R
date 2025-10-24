## Stratifikovany vyber
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


# Soubor Mzdy.txt obsahuje udaje o hrubych mzdach a vzdelani N=1134 zamestnacu. 
# Vzdelani je kodovano nasledovne:
# 1 .... ZS vzdelani
# 2..... SS vzdelani bez maturity
# 3..... SS vzdelani s maturitou
# 4..... VS vzdelani
## Nacteni dat o mzdach:
getwd()
setwd("C:/Users/vozar9250/Downloads")
setwd("C:/Users/vozo01/Downloads")

vstup=read.table("Mzdy.txt",header=TRUE,sep=",")
names(vstup)
summary(vstup)

## Ma cenu uvazovat stratifikaci?


boxplot(Mzda~vzdel,data=vstup)

boxplot(Mzda~as.character(vzdelani),data=vstup)

## Rozklad celkove variability vnitroskupinovou a meziskupinovou variabilitu
## chyba, identifikator strata musi byt typu character
summary(aov(Mzda~vzdel,data=vstup))
## Ted je to spravne
summary(aov(Mzda~as.character(vzdel),data=vstup))
SBG=summary(aov(Mzda~as.character(vzdel),vstup)) [[1]][1,2]
SWG=summary(aov(Mzda~as.character(vzdel),vstup)) [[1]][2,2]
SST=SBG+SWG
## Podil meziskupinove na celkove variabilite
SBG/SST


## alternativne pomoci funkce lm

## ktere je spravne?

## vzdelani jako ciselna hodnota?
summary(lm(Mzda~vzdel,data=vstup))

## vzdelani jako promenna typu character?
summary(lm(Mzda~as.character(vzdel),data=vstup))



## Upravime si data frame s populaci

Populace=data.frame(as.character(vstup$vzdel),vstup$Mzda)
colnames(Populace)=c("vzdel","mzdy")

names(Populace)
summary(Populace)


## Pocet strat a ktera
length(unique(Populace$vzdel))
unique(Populace$vzdel)
## Pocet jednotek v stratech populace - dle vzdelani
popsize=table(Populace$vzdel)
popsize
dim(popsize)
## populacni rozptyly mez ve stratech

install.packages("plyr")
library(plyr)

ddply(Populace,~vzdel,summarise,var=var(mzdy))

ddply(Populace,~vzdel,summarise,var=var(mzdy))$var
ddply(opulace,~vzdel,summarise,var=var(mzdy))$vzdel

popvar=ddply(Populace,~vzdel,summarise,var=var(mzdy))$var
names(popvar)=ddply(Populace,~vzdel,summarise,var=var(mzdy))$vzdel
popvar

# relativni naklady na osloveni jednotky ve stratech

relcost=c(1.0,1.2,1.1,1.8)
names(relcost)=ddply(Populace,~vzdel,summarise,var=var(mzdy))$vzdel
# Pocet strat
L=length(unique(Populace$vzdel))
L
# Vyberte n=200 jednotek
n=200
n
# rovnomerna alokace

rep(n/L,times=L)
unif_aloc=round(rep(n/L,times=L))
unif_aloc
names(unif_aloc)=unique(Populace$vzdel)
unif_aloc
# Proporcionalni alokace
n*popsize/sum(popsize)

prop_aloc=round(n*popsize/sum(popsize))
prop_aloc

## Neymanova (optimalni alokace s konst. naklady osloveni v kazdem stratu

n*popsize*sqrt(popvar)/sum(popsize*sqrt(popvar))

neym_aloc=round(n*popsize*sqrt(popvar)/sum(popsize*sqrt(popvar)))
neym_aloc

## optimalni alokace s ruznymi naklady osloveni v kazdem stratu

opt_aloc=round(n*popsize*sqrt(popvar/relcost)/sum(popsize*sqrt(popvar/relcost)))
opt_aloc
## vyber pomoci package sampling

## populace musi byt usporadana podle promenne pro definici strat

order(Populace$vzdel)

Populace_ord=Populace[order(Populace$vzdel),]

## provedeni vyberu
## index_unif - por cisla vybranych zamestnancu dle rovnomerne alokace
index_unif=strata(Populace,stratanames=c("vzdel"),size=unif_aloc,method="srswor")
index_unif
## kolik jsme vybrali zamestnancu?
dim(index_unif)

## vybereme data pro vybrane zamestnace
sample_unif=getdata(Populace_ord,index_unif)
sample_unif
## dulezity je sloupce Prob, to se puuzije k vypoctu vah.
sample_unif$vaha=1/sample_unif$Prob
sample_unif$vaha

## kontrola suma vah ve stratu se musi rovnat velikosti populace Nh ve stratu
tapply(sample_unif$vaha,sample_unif$vzdel,sum)
popsize

## jeste se do vyberu musi pridat sloupec pro fpc = velikost populace ve stratu Nh
popsize_recode=c("1"=125,"2"=442,"3"=408,"4"=159)
sample_unif$fpc=popsize_recode[sample_unif$vzdel]
sample_unif
# kontrola promenne fpc
table(sample_unif$fpc)

## vytvori se survey design
sd_unif=svydesign(id=~1,strata=~vzdel,weights=~vaha,fpc=~fpc,data=sample_unif)

## odhad prumerne mzdy za celou populace
svymean(~mzdy,sd_unif)


## jak pristupovat k hodnotam
## vyb. prumer
svymean(~mzdy,sd_unif)[[1]]
coef(svymean(~mzdy,sd_unif))
## standard error
SE(svymean(~mzdy,sd_unif))


## intervaly spolehlivosti pro populacni prumer

confint(svymean(~mzdy,sd_unif))
confint(svymean(~mzdy,sd_unif),level=0.95)
confint(svymean(~mzdy,sd_unif),level=0.99)


## odhady prumeru v jednotlivych stratech
svyby(~mzdy,by=~vzdel,sd_unif,svymean,keep.var=TRUE)

## interval spolehlivosti
confint(svyby(~mzdy,by=~vzdel,sd_unif,svymean,keep.var=TRUE))

## chceme vytahnout odhady prumeru ve stratech
coef(svyby(~mzdy,by=~vzdel,sd_unif,svymean,keep.var=TRUE))
## chceme vytahnout sm. odchylku (SE) ve stratech
SE(svyby(~mzdy,by=~vzdel,sd_unif,svymean,keep.var=TRUE))

## odhady kvantilu
## empiricke
svyquantile(~mzdy,sd_unif,quantiles=c(0.1,0.25,0.50,0.75,0.9),ties="discrete")
## s interpolaci (preferovane)
svyquantile(~mzdy,sd_unif,quantiles=c(0.1,0.25,0.50,0.75,0.9),ties="rounded")


## chceme odhadnout empirickou distribucni funkci mezd za celou populaci
## zamestnanci v ruznych stratech jsou vybrani s ruznou pravdepodobnosti
## nelze pouzit prikaz ecdf bez uprav
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(ecdf(sample_unif$mzdy))
## spravne
## populacni emp. distr. fce
ecdfpop=ecdf(Populace$mzdy)
ecdfsamp=svycdf(~mzdy,sd_unif)
plot(ecdfpop,do.points=FALSE,xlab="Mzdy v Kc",main="Populacni a vyberova ECDF")
lines(ecdfsamp[[1]],col="red",do.points=FALSE)

## histogram - vazeny

svyhist(~mzdy,sd_unif,xlim=c(0,50000),ylim=c(0,0.0001))
## jadrovy odhad hustoty
lines(svysmooth(~mzdy,sd_unif,bw=500),lwd=2)

## porovnejte jednotlive alokace vyberu z hlediska presnosti odhadu celkove prumerne mzdy
## jaky vyber je vhodny, pokud chcete krome dostatecne presneho odhadu celkove
 prumerne mzdy je treba mit i pomerne presne odhady prumeru mezd dle vzdelani


## balicek dplyr lze vyuzit k rovnomernemu a proporcionalnimu vyberu
 install.packages("dplyr")
library("dplyr")

## rovnomerny vyber, 20 zamestnancu z kazdeho strata
set.seed(321)
s1=Populace%>%group_by(vzdel)%>%sample_n(20)
s1
print(s1,n=80)
names(s1)
## proporcionalni vyber, 10% vyber v kazdem stratu
set.seed(321)
s2=Populace%>%group_by(vzdel)%>%sample_frac(0.1)
s2
print(s2,n=113)
names(s2)
