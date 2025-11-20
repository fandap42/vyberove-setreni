### Iterative Proprortional Fitting: Knihovna anesrake
### uklid
rm(list=ls())
### Instalace knihovny
install.packages("anesrake")
### Spusteni knihovny
library(anesrake)
### data anes04
data(anes04)
names(anes04)
summary(anes04)

### Vytvorime id pozorovani caseid
anes04$caseid <- 1:length(anes04$age)
anes04$caseid 

### Budeme narovnat na demografickou strukturu (pohlavi, vek)
### Vytvorime vekove kategorie
anes04$agecats <- cut(anes04$age, c(0, 25,35,45,55,65,99))
anes04$agecats 
levels(anes04$agecats) <- c("age1824", "age2534", "age3544",
"age4554", "age5564", "age6599")

### Zadame demografickou strukturu (relativne)
### Jen marginalni strukturu (jednorozmerna)
marriedtarget <- c(.4, .6)
agetarg <- c(.10, .15, .17, .23, .22, .13)
names(agetarg) <- c("age1824", "age2534", "age3544",
"age4554", "age5564", "age6599")

marriedtarget

### tvorba vektoru se strukturou,na kterou narovnavame vyber
targets <- list(marriedtarget, agetarg)

### Vektory v seznamu vektoru targets se musi jmenovat jako promenne ve dataframe obsahujici vyberovy soubor
names(targets) <- c("married", "agecats")

### Aplikujeme raking, verbose=TRUE vypis jednotlivych iteraci
outsave <- anesrake(targets, anes04, caseid=anes04$caseid,
verbose=TRUE)
### Ulozime si vahy
caseweights <- data.frame(cases=outsave$caseid, weights=outsave$weightvec)
### Summary vysledku
summary(caseweights)
summary(outsave)
caseweights

## Promenna weights z data frame caseweights se prida do data frame anes04
## Pomoci teto vahy se pak prevazi vysledky (ruzne tabulky) z vyberu v data frame anes04
anes04$weights=caseweights$weights