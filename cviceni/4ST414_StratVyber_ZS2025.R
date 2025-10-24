# # # 4ST414, Strafikovany vyber, prum. mzda, primy vypocet


# Soubor Mzdy.txt obsahuje udaje o hrubych mzdach a vzdelani N=1134 zamestnacu. 
# Vzdelani je kodovano nasledovne:
# 1 .... ZS vzdelani
# 2..... SS vzdelani bez maturity
# 3..... SS vzdelani s maturitou
# 4..... VS vzdelani

# Nahodne vyberte n=200 jednotek a odhadnete prumernou mzdu v tomto souboru
# Provedte strafitikovany nahodny vyber s rovnomerou alokaci (v kazde skupine vzdelani vybere 50 zam.)
# Odhadnete prumernou mzdu jak pro skupiny zamestnancu s ruznym vzdelavanim, tak za populaci celkem
# Vypoctete dvoustranne intervalu spolehlivosti pro zvolenou spolehlivost 1-alfa=0.95
# Porovnejte s prostym nahodnym vyberem o rozsahu n=200

setwd("C://Users/vozar9250")
getwd()
mzdy<-read.table("Mzdy.txt",sep=",",header=TRUE)
names(mzdy)
summary(mzdy)
# Zobrazime mzdovou uroven dle vzdelani
boxplot(Mzda~vzdel,data=mzdy)
# Lisi se uroven mezd dle vzdelani?
summary(aov(Mzda~vzdel,data=mzdy))
summary(aov(Mzda~as.character(vzdel),data=mzdy))


# Urcime velikost populace N
N=length(mzdy$Mzda)
N
# Urcime pocet strat L
L=length(unique(mzdy$vzdel))
L
# Urcime nazvy strat:
stratum_jm=seq(1:L)
stratum_jm
# Urcime velikost populace Nh v jednotlivy stratech h=1,2,...,L
t=as.data.frame(table(mzdy$vzdel))
t
Nh=t[,2]
Nh
# Stanovime rozsah vyberu nh v jednotlivy stratech h=1,2,...,L
nh=rep(50,L)
nh
n=sum(nh)

# Incializace vektoru pro mezivysledky a vysledky
# m_y_h: vyberove prumery v jednotlivych stratech
# var_y_h: vyberove rozpyly v jednotlivych stratech
# Y_hat_h: odhad prumernych mezd v jednotlivych stratech
# D_Y_hat_h: odhad rozptylu prumernych mezd v jednotlivych stratech
# Y_hat: odhad celkove prumerne mzdy
# D_Y_hat: odhad rozpylu prumerne mzdy


m_y_h=rep(0,L)
var_y_h=rep(0,L)
Y_hat_h=rep(0,L)
D_Y_hat_h=rep(0,L)

# vypocet alfa ze spolehlivosti pro interval spolehlivosti:
spol=0.95
alfa=1-spol
for (i in 1:L) {
  Uh=t(subset(mzdy,vzdel==stratum_jm[i],select=Mzda))
  sh=sample(Uh,size=nh[i],replace=FALSE)
  m_y_h[i]=mean(sh)
  var_y_h[i]=var(sh)
  Y_hat_h[i]=m_y_h[i]
  D_Y_hat_h[i]=(1-nh[i]/Nh[i])*var_y_h[i]/nh[i]
  }

# odhad celkove prum mzdy:
Y_hat=sum(Nh*Y_hat_h)/N
print(c(paste("Odhad prumerne mzdy cini: "),round(Y_hat)))
# odhad rozptylu odhadu prum. mdzy
D_Y_hat=sum(Nh*Nh*D_Y_hat_h)/N**2
CI.d<-Y_hat-qt(1-alfa/2,n-1)*sqrt(D_Y_hat)
CI.h<-Y_hat+qt(1-alfa/2,n-1)*sqrt(D_Y_hat)
CI=c(CI.d,CI.h)
print(c(paste(spol*100,"% interval spolehlivosti prumerne mzdy cini: ")))
print(round(CI))

# odhad  prum mzdy dle vzdelani
CIh.d<-Y_hat_h-qt(1-alfa/2,nh-1)*sqrt(D_Y_hat_h)
CIh.h<-Y_hat_h+qt(1-alfa/2,nh-1)*sqrt(D_Y_hat_h)

for (i in 1:L) {
  print(c(paste("V stratu ", stratum_jm[i]," odhad prumerne mzdy cini: "),round(Y_hat_h[i])))
  print(c(paste(spol*100,"% interval spolehlivosti prumerne mzdy cini: ")))
  print(round(c(CIh.d[i],CIh.h[i])))
  }

