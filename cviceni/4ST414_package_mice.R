## Instaluj potrebne balicky
install.packages("mice")
install.packages("lattice")
library(mice)
library(lattice)

## maly soubor nhanes
nhanes
names(nhanes)


## Oznaceni, zda je pozorovani uplne
complete.cases(nhanes)

## Vyber kompletnich pozorovani
subset(nhanes,complete.cases(nhanes)==TRUE)

## kolik jich chybi
summary(nhanes)

## vzory chybejicich promennych
md.pattern(nhanes)

## soubor boys
boys
names(boys)


## Oznaceni, zda je pozorovani uplne
complete.cases(boys)

## Vyber kompletnich pozorovani
subset(boys,complete.cases(boys)==TRUE)

## kolik jich chybi
summary(boys)

## vzory chybejicich promennych
md.pattern(boys)


## jak na imputaci
## funkce mice
## parametry
## m = pocet imputaci pro vicenasobnou imputaci (default m=5)
## maxit = pocet iteraci (default maxit=5)
## method: "mean" prumer, "norm.predict" predikce regresi, "norm.nob" stochasticka regrese
## seed: seed

## imputace nhanes 

## prumer
imp1=mice(nhanes,method="mean",m=1,maxit=1,seed=123)
## predikce regresi
imp2=mice(nhanes,method="norm.predict",m=1,maxit=1,seed=123)
## predikce stochastickou regresi
imp3=mice(nhanes,method="norm.nob",m=1,maxit=1,seed=123)

## zobrazeni imputovanych metod
## striplot, na vodorovne ose cislo imputace, modre data, cervene imputovana data
stripplot(imp1)
stripplot(imp2)
stripplot(imp3)
## bwplot totez, ale boxploty
bwplot(imp1)
bwplot(imp2)
bwplot(imp3)
## hustoty
densityplot(imp1)
densityplot(imp2)
densityplot(imp3)

## mnohonasobna imputace
imp4=mice(nhanes,m=5,maxit=5,seed=123)
stripplot(imp4)
bwplot(imp4)
densityplot(imp4)

## zkuste dataset boys nebo leiden
