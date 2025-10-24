## 4ST414 
## Domenovy odhad
## Kombinace stratifikovaneho a pomeroveho, regresniho odhadu

## Priprava prostredi a nacteni erkovych balicku
## Stratifikovany vyber
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

## Vse se bude ilustrovat na souboru svedskych municipalit MU284
data(MU284)
summary(MU284)
## Omezime se je municipality s mene nez 100 tis. obyv v roce 1975 (P75<100)
MU284pod100=MU284[MU284$P75<100,]
summary(MU284pod100)
## Domenu si definujeme jako municipality s mene nez 30 tis. obyv. v roce 
MU284pod100$domena=ifelse(MU284pod100$P75<30,"small","medium")
## Vybereme 10% municipalit
N=length(MU284pod100$REG)
N
n=floor(0.1*N)
n
# Vybereme 10% jednotek
s1=sample(N,n,replace=F)
s1
# Vyberovy soubor s1
Vyber1=MU284pod100[s1,]
Vyber1
# Do vyberu pridame sloupce s N (popsize) a vahy (N/n)
Vyber1$popsize=rep(N,times=n)
Vyber1$vahy=rep(N/n,times=n)
## Definice survey desing pro 10% vyber
sd1=svydesign(id=~1,weights=~vahy,fpc=~popsize,data=Vyber1)
## Definice survey designu pro male municipality
sd1small=subset(design=sd1,domena=="small")
## Odhad danovych prijmu pro male obce
totalRMT85small=svytotal(~RMT85,design=sd1small)
totalRMT85small
## pocet stupnu volnosti = pocet municipalit ve vyberu - 1
df1=sum(Vyber1$domena=="small")-1
df1
## 99% interval spolehlivosti pro uhrn
confint(totalRMT85small,level=0.99,df=df1)

## Chceme domenove odhady za populaci celkem
domtot=svyby(~RMT85,by=~factor(domena),design=sd1,svytotal)
domtot
confint(domtot)

## Chceme domenove odhady domeny jednotlive regiony

domtotreg=svyby(~RMT85,by=~factor(domena)+factor(REG),design=sd1,svytotal)
domtotreg
confint(domtotreg)

## Proc jsou regionalni odhady tak nepresne?

## Kombinace stratifikovaneho vyberu a pomeroveho/regresniho odhadu
## Opet odhadujeme danove prijmy
## Vybereme 60 municipalit pomoci proporcionalni alokace, stratum je region
MU284pod100$REG=as.character(MU284pod100$REG)
## Pocet strat a ktera
length(unique(MU284pod100$REG))
unique(MU284pod100$REG)
## Pocet jednotek v stratech populace - regionu
popsize=table(MU284pod100$REG)
popsize
dim(popsize)
## Bude stratifikace vyberu dle regionu prinosna?


## Rozsah vyberu je 60
n=60
## Propocionalni alokace vyberu
## provedu vyber
round((n*popsize/sum(popsize)))
## Proc tato uprava vyberu?
pmin(pmax(round((n*popsize/sum(popsize))),2),popsize)

prop_aloc=pmin(pmax(round((n*popsize/sum(popsize))),2),popsize)
prop_aloc
sum(prop_aloc)

## populace musi byt usporadana podle promenne pro definici strat

order(MU284pod100$REG)

MU284pod100_ord=MU284pod100[order(MU284pod100$REG),]

## provedeni vyberu
## index_prop - por cisla vybranych municipalit dle proporcionalni alokace
index_prop=strata(MU284pod100_ord,stratanames=c("REG"),size=prop_aloc,method="srswor")
index_prop
## kolik jsme vybrali municipalit?
dim(index_prop)

## vybereme data pro vybrane municipality
sample_prop=getdata(MU284pod100_ord,index_prop)
sample_prop
## dulezity je sloupce Prob, to se puuzije k vypoctu vah.
sample_prop$vaha1=1/sample_prop$Prob
sample_prop$vaha1

## kontrola suma vah ve stratu se musi rovnat velikosti populace Nh ve stratu
tapply(sample_prop$vaha1,sample_prop$REG,sum)
popsize



## jeste se do vyberu musi pridat sloupec pro fpc = velikost populace ve stratu Nh
popsize_recode=c("1"=24,"2"=43,"3"=31,"4"=36,"5"=54,"6"=41,"7"=15,"8"=29)
sample_prop$fpc=popsize_recode[sample_prop$REG]
sample_prop$fpc
# kontrola promenne fpc
table(sample_prop$fpc)

## vytvori se survey design
sd_prop=svydesign(id=~1,strata=~REG,weights=~vaha1,fpc=~fpc,data=sample_prop)

## pomerovy odhad I - spoctu odhad pomeru za celou populaci pomoci stratifikvoaneho vyberu
## pomerovy odhad RMT85 k P75
ratio_total=svyratio(~RMT85,~P75,design=sd_prop)

ratio_total
confint(ratio_total)


#odhad uhrnu RMT85
# musim mit populacni uhrn P75
P75tot=sum(MU284pod100$P75)
# populacni uhrn RMT85 pro kontrolu
RMT85tot=sum(MU284pod100$RMT85)

# odhad uhrnu RMT85
predict(ratio_total,total=P75tot)
RMT85tot

## Pomerovy vyber II - vypoctu pomery RMT85 a P75 v stratech (regionech REG)
## pak odhadnu uhrny RMT85 v jednotlivych regionech a zkombinuju
## k tomu slouzi parametr separate
ratio_separate=svyratio(~RMT85,~P75,design=sd_prop,separate=TRUE)
ratio_separate
## musime mit populacni uhrny P75 v regionech
aggregate(MU284pod100$P75,list(MU284pod100$REG),sum)

## ulozime do vektoru pro nafitovani odhadu
stratum.totals=list("1"=817,"2"=799,"3"=658,"4"=815,"5"=1056,"6"=860,"7"=399,"8"=497)

## Odhad RMT85 pomoci II. metody
predict(ratio_separate,stratum.totals)
RMT85tot

## Regresni odhad

## regresni odhad I - odhad jednoho regresniho modelu pro celou populaci
## Model 1: y=a+bx
model1=svyglm(RMT85~P75,design=sd_prop)
model1
confint(model1)
## odhad uhrnu
## dataframe pro hodnotu pomocne promenne P85
newdata=data.frame(P75=P75tot)
## Odhad uhrnu, do promene total dat rozsah populace
predict(model1,newdata,total=length(MU284pod100$RMT85))
## Intervaly spol. opet pomoci funkce confint
confint(predict(model1,newdata,total=length(MU284pod100$RMT85)))






