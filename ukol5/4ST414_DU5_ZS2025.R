## data ----
# Soubor testyDU5.csv obsahuje simulované údaje o počtu získaných bodů ze zkoušky u M=1600 studentů
# celoškolského povinného kvantitativního předmětu. Soubor obsahuje tyto proměnné:
# • id_s: identifikátor studenta
# • id_cv: identifikátor cvičení (80 cvičení po 20 studentech)
# • typ: typ cvičení (1: kvantitativní obory, 2: ekonomické obory, 3: humanitní obory
# • body: počet získaných bodů.

data <- read.csv("testyDU5.csv")
data$typ <- as.factor(data$typ)
summary(data)

M <- 1600

# vyber ----
# Vedení školy chce získat rychlý odhad průměrného počtu bodů u zkoušky, dostupné kapacity jsou 
# na opravení 1/10 testů. Připadají v úvahu tyto možnosti:
# a) vybereme 1/10 studentů ----
smpl_a <- sample(data$id_s, size=M/10) # asi to chce jen radky z data

# b) v cvičeních každého typu vybereme 1/10 studentů ----
aloc <- table(data$typ)/10
b <- sampling::strata(data, stratanames = c("typ"), size=aloc, method="srswor", description = T)
vybb <- sampling::getdata(data, b)
dim(vybb)

# c) vybereme 1/10 cvičení a ty kompletně opravíme ----
c <- sampling::cluster(data,clustername=c("id_cv"),size=8,method="srswor")
vybc <- sampling::getdata(data,c)
dim(vybc)
# survey_c=svydesign(id=~CT,weights=~vaha,fpc=~fpc,data=vybc)

# d) vybereme 1/5 cvičení a zde opravíme ½ testů ----
cviceni <- unique(data$id_cv)
d <- sampling::mstage(data,stage=list("cluster", ""),varnames=list("id_cv"),size=list(length(cviceni)/10,10),method=c("srswor","srswor"))
dim(d[1])
vybd=getdata(data,d[1])
dim(vybd)
# survey_d=svydesign(id=~CT,weights=~vaha,fpc=~fpc,data=vybd)


# simulace ----
# Před každou simulací nastavte seed jako RRRRMMEE z vašeho data narození.
# i) Pro všechny možnosti a) - d) zvolte vhodný výběrový plán a uveďte, ----
#    zda bude bodový odhad populačního průměru nestranný (1 b)
# ii) Pro všechny možnosti a) – d) spočtěte teoretickou směrodatnou odchylku odhadu průměru ----
#     a zvolte vhodný výběrový plán. Pokud máte dodatečnou informaci, že v cvičeních jsou přibližně
#     stejně dobří studenti, je skupinkový a vícestupňový rozdíl vhodný (3 b)?
# iii) Pro R=500 replikací proveďte simulaci všech 4 možností a porovnejte graficky jejich přesnost(1 b) ----



