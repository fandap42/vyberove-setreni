############
#Dvoustupnovy vyber/two-stage sampling
############

##Datovy soubor testy.csv: simulovana data vysledku testu celoskolskeho predmetu
## 80 cviceni po 20 studentech, tj. M=1600, Mi=20, N=80.
##Promenne
##id_s: identifikator studenta (SSU (secondary sampling unit) - student)
##id_cv: identifikator cviceni (PSU (primary sampling unit)- skupina je cviceni)
##typ: typ cviceni (1: kvantitativni obory, 2: ekonomicke obory, 3: humanitni obory)
##body: bodove ohodnoceni: 0-20 bodu

##nacteni souboru
setwd("C:/Users/Vozar")
getwd()
testy<-read.csv("testy.csv")
names(testy)
summary(testy)
##nastaveni promennych
M<-1600 ##rozsah ZS, pocet sekundarnich jednotek v ZS, pocet studentu
N<-80 ## pocet primarnich jednotek (jednotl. cviceni v ZS)
Mi<-20 ## pocet studentu na 1 cviceni v ZS
PSU_jm=seq(1:N) ## nazev PSU
##chceme vybrat celkem 1/10 studentu, opravime testy odhadneme prumerny pocet bodu z testu
##Vybereme 1/5, tj. 16 cviceni a v kazdem z nich opravime 1/2 testu, tj. mi=konst=10
n<-ceiling(N/5)
mi<-ceiling(Mi/2)

PSU_vyb=sample(PSU_jm,n,replace=F) ## nazev PSU

## v_Mi: vektor poctu SSU v i-te PSU
v_Mi=rep(Mi,N)
## v_mi: vektor  SSU ve vybranych PSU
v_mi=rep(mi,n)



## Odhadnete prumerny bodovy zisk a 95% oboustranny interval spolehlivosti

# Incializace vektoru pro mezivysledky a vysledky
# m_y_i: vyberove prumery v jednotlivych PSU
# var_y_i: vyberove rozpyly v jednotlivych PSU
# D_Y_hat_i: odhad rozptylu uhrnu v jednotlivych PSU
# Y_hat_i: odhad uhrnu v jednotlivych PSU
# mY_hat: odhad celkove prumerum na 1 SSU
# D_mY_hat: odhad rozpylu prumeru na 1 SSU


m_y_i=rep(0,n)
var_y_i=rep(0,n)
Y_hat_i=rep(0,n)
D_Y_hat_i=rep(0,n)

# vypocet alfa ze spolehlivosti pro interval spolehlivosti:
spol=0.95
alfa=1-spol
# provadi se vybery v jednotlivych PSU, vypoctou se vyb. prumery a rozptyly uhrnu v i-te PSU
for (i in 1:n) {
  Ui=t(subset(testy,id_cv==PSU_vyb[i],select=body))
  si=sample(Ui,size=v_mi[i],replace=FALSE)
  m_y_i[i]=mean(si)
  var_y_i[i]=var(si)
  Y_hat_i[i]=m_y_i[i]*v_Mi[PSU_vyb[i]]
  D_Y_hat_i[i]=v_Mi[PSU_vyb[i]]*(v_Mi[PSU_vyb[i]]-v_mi[i])*var_y_i[i]/v_mi[i]
  }

# odhad prumeru na SSU:
mY_hat=N*sum(Y_hat_i)/(n*M)
print(c(paste("Odhad prumerne mzdy cini: "),round(mY_hat,1)))
# odhad prumeru na PSU:
mu1_hat=mean(Y_hat_i)
# odhad rozptylu odhadu prum. mdzy
D_mY_hat=(N*(N-n)*var(Y_hat_i)/n+N*sum(D_Y_hat_i)/n)/M**2
CI.d<-mY_hat-qt(1-alfa/2,n-1)*sqrt(D_mY_hat)
CI.h<-mY_hat+qt(1-alfa/2,n-1)*sqrt(D_mY_hat)
CI=c(CI.d,CI.h)
print(c(paste(spol*100,"% interval spolehlivosti prumeru na SSU cini: ")))
print(round(CI,1))

# Upravte kod pro vyber skupin, vybirame 1/10 cviceni.

##Nejprve si spocteme stredni hodnotu a teoreticky rozptyl estimatoru

E_t_y_sk=mean(testy$body)

## Pripravne vypocty pro teor. roztyl
## prumerny bodovy zisk ve cvicenich
prum_cv<-tapply(testy$body, testy$id_cv, sum)
## vyber. rozptyl bodoveho zisk ve cvicenich
vars_cv<-tapply(testy$body, testy$id_cv, var)
## rozptyl prum. bodoveho zisku ve cvicenich
vars_prum_cv<-var(prum_cv)
Var_t_y_sk=(N*(N-n)*vars_prum_cv/n+(N/n)*Mi*(Mi-mi)*sum(vars_cv/mi))/M**2




