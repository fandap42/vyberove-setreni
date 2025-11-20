# # Uklid v R
rm(list = ls());

# Pouzivana R library sampling

install.packages("sampling")
library(sampling)

# Pr. 1: 2 x hazeme pravidelnou desetistennou kostkou
# a) Urcete zakladni soubor
# b) Jedna se o vyber se stejnymi nebo ruznymi pravdepodobnostmi?
# c) Jedna o vyber bez vraceni nebo s vracenim?
# d) Urcete vektor pravdepodobnosti pi, ze dane cislo padne v i-tem tahu
# e) Urcete vektor pravdepodobnost phi, ze ve 2 tazich padne aspon jedno cislo.
# f) Urcete nejmensi rozsah vyber, tj. pocet hodu, aby pravdepodnost zahrnuti do vyberu dosahla aspon 0.9.

# Pr. 2: 3 x hazene nepravidelnou desetistennou kostkou.
# Pravdepodobnost, ze padne i, i=1,2,...,10 je primo umerna velikosti cisla i
# a) Urcete zakladni soubor
# b) Jedna se o vyber se stejnymi nebo ruznymi pravdepodobnostmi?
# c) Jedna o vyber bez vraceni nebo s vracenim?
# d) Urcete vektor pravdepodobnosti pi, ze dane cislo padne v i-tem tahu
# e) Urcete vektor pravdepodobnost phi, ze ve 2 tazich padne aspon jedno cislo.

# Pr. 3: Populace U ma 100 jednotek, a to 1,2,...,99,10000
# Urcete pravdpodonosti vyberu v danem tahu umerne velikosti pro 10% vyber.
# Proc nize uvedeny vzorec nefunguje?

U=c(1:99,10000)
N=length(U)
N
n=0.1*N
n
pi=n*U/sum(U)
pi
summary(pi)
max(pi)

# funkce inclusionprobabilities
pi1=inclusionprobabilities(U,n)
pi1
sum(pi1)

# Urcete vektor pravdepodobnosti zahrnuti do vyberu o rozsahu n

# Provedte je vyber s timto vyberovym planem.

help(sample)
sample(U,n,replace=F,pi1)


# Pr. 4, provadime prosty nahodny vyber s vracenim, vybereme vzdy polovinu jednotek
# Urcete pravdepodobnost zahrnuti do vyberu pro obecne N a n
# Jaka je tato pravdepodobnost, pokud je setrena populace velmi velka.

## Data o belgickych municipalitach
data(belgianmunicipalities)
names(belgianmunicipalities)
dim(belgianmunicipalities)

## pristup k jednotlivym promennych
attach(belgianmunicipalities)
## zajimaji nas promenne Tot3 - pocet obyv. v roce 2003
## Totaltaxation - danove vynosy v roce 2004

## N: velikost populace
N=length(Tot03)
N
## ruzne kombinace vyberu bez vraceni
## prosty nahodny vyber bez vraceni n=0.25*N
n=round(0.25*N)
n

## pravdepodobnosti zahrnuti do vyberu 1. radu
pik1=rep(n/N,times=N)
sum(pik1)
length(pik1)

## matice psti zahrnuti 2. radu
pikl1=matrix(rep(n*(n-1)/(N*(N-1)),times=N*N),nrow=N)
diag(pikl1)=pik1
## Nahodny vyber - hodnoty Totaltaxation
set.seed(123)
s1=sample(N,n,replace=F)
vyb1=Totaltaxation[s1]
vyb1

## bernouliovsky vyber - s psti 0.25 vyberu danou jednotku, vyber provadim nezavisle
pik2=rep(0.25,times=N)
sum(pik2)
length(pik2)

## matice psti zahrnuti 2. radu
pikl2=outer(pik2,pik2,"*")
dim(pikl2)
diag(pik2)=pik2

## Nahodny vyber - hodnoty Totaltaxation
set.seed(123)
s2=UPpoisson(pik2)
vyb2=Totaltaxation[s2==1]
vyb2



## Jakym rozdelenim se ridi rozsah vyberu touto metodou?

## Proporcionalni vyber. psti proporcionalni dle velikosti Tot3
psti_prop=inclusionprobabilities(Tot03,n)
sum(psti_prop)
length(psti_prop)

## Poissonovsky vyber
pik3=psti_prop
sum(pik3)
length(pik3)

## matice psti zahrnuti 2. radu
pikl3=outer(pik3,pik3,"*")
dim(pikl3)
diag(pikl3)=pik3

## Nahodny vyber - hodnoty Totaltaxation
set.seed(123)
s3=UPpoisson(pik3)
vyb3=Totaltaxation[s3==1]
vyb3

## Proporcionalni prosty nahodny vyber bez vraceni
pik4=psti_prop
sum(pik4)
length(pik4)

## matice psti zahrnuti 2. radu
## Neumime

## Nahodny vyber - hodnoty Totaltaxation
set.seed(123)
s4=sample(N,n,prob=pik4,replace=F)

vyb4=Totaltaxation[s4]
s4

## Odhady uhrnu Totaltaxation:

## 1. metoda
## Odhad uhrnu - Horvitz-Thompson
HTestimator(vyb1,pik1[s1])
sum(Totaltaxation)
## Odhad rozptylu
## Nestranny odhad
varHT(vyb1,pikl1[s1,s1])
## Proc se to shoduje s timto vyrazem?
N*(N-n)*var(vyb1)/n
## Devilleho metoda - staci znat jen pik, ne matici psti zahrnuti druheho radu
varest(vyb1,pik=pik1[s1])

## 2. metoda
## Odhad uhrnu - Horvitz-Thompson
HTestimator(vyb2,pik1[s2==1])
sum(Totaltaxation)
## Odhad rozptylu
## Nestranny odhad - defaultni metoda je lepsi pro vyber s nahodnou velikosti vyberu
varHT(vyb2,pikl2[s2==1,s2==1])

## Devilleho metoda - staci znat jen pik, ne matici psti zahrnuti druheho radu - tady je silne podhodnoceni
varest(vyb2,pik=pik2[s2==1])

## 3. metoda
## Odhad uhrnu - Horvitz-Thompson
HTestimator(vyb3,pik3[s3==1])
sum(Totaltaxation)
## Odhad rozptylu
## Nestranny odhad
varHT(vyb3,pikl[s3==1,s3==1])

## Devilleho metoda - staci znat jen pik, ne matici psti zahrnuti druheho radu
varest(vyb3,pik=pik3[s3==1])

## 4. metoda
## Odhad uhrnu - Horvitz-Thompson
HTestimator(vyb4,pik4[s4])
sum(Totaltaxation)
## Odhad rozptylu
## Nestranny odhad - neumime


## Devilleho metoda - staci znat jen pik, ne matici psti zahrnuti druheho radu
varest(vyb4,pik=pik4[s4])










