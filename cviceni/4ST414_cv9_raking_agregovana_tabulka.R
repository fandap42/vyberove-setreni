### Priklad na raking, iterative proportion fitting
### Setreni mezi 10 000 studenty, studujicich na 4 fakultach: F1,F2,F3 a F4
### Tri typy studia: bak: bakalarske, ing: inzenyrske, MBA: MBA
### Na jednotlivych fakultach studuje 1000, 2000, 3000 a 4000 
### V jednotlivych stupnich studuje 4500, 3500 a 2000 studentu.
### V matici odh.struktury jsou vazene odhady poctu studentu studujici na ruznych fakultach ruzne typy studia
### V radcich tabulky jsou obory studia, ve sloupcich fakulty
odh.struktury=matrix(c(200,800,600,3000,500,1000,1200,1300,500,450,550,400),nrow=3,byrow=TRUE)
### I - pocet radku, J - pocet sloupcu
I=3
J=4
odh.struktury
### Pridame nazvy radku
row.names(odh.struktury)=c("bak","ing","MBA")
colnames(odh.struktury)=c("F1","F2","F3","F4")
odh.struktury

### Populacni radkove soucty - kolik studentu studuje stupen studia
PopRadek=c(4500,3500,2000)
### Populacni sloupcove soucty - kolik studentu studuje fakultu
PopSloupec=c(1000,2000,3000,4000)

### 1. iterace
### odhad sumy pomocne promenne
sum(odh.struktury)
### radkove soucty
SampRadek=rowSums(odh.struktury)
# porovnani radkovych soucty
PopRadek-SampRadek
### vypocet prvni iterace

odh.struktury1=odh.struktury*rep(t(PopRadek/SampRadek),J)
odh.struktury1
### kontrola radkovych souctu
rowSums(odh.struktury1)
rowSums(odh.struktury1)-PopRadek
### kontrola sloupcovych souctu
colSums(odh.struktury1)
colSums(odh.struktury1)-PopSloupec
sum(colSums(odh.struktury1))
### prevazene odhady uhrnu populacni sumy
sum(odh.struktury1)
sum(odh.struktury)

### 2. iterace
### odhad sumy pomocne promenne
sum(odh.struktury1)
### sloupcove soucty
SampSloupec=colSums(odh.struktury1)
SampSloupec
sum(SampSloupec)
# porovnani sloupcovych soucty
PopSloupec-SampSloupec
sum(PopSloupec-SampSloupec)
### vypocet druhe iterace

odh.struktury2=odh.struktury1*matrix(rep((PopSloupec/SampSloupec),I),nrow=I,byrow=TRUE)
odh.struktury2
sum(odh.struktury2)
### kontrola radkovych souctu

sum(rowSums(odh.struktury2)-PopRadek)
rowSums(odh.struktury2)-PopRadek
### kontrola sloupcovych souctu
colSums(odh.struktury2)
sum(colSums(odh.struktury2)-PopSloupec)

colSums(odh.struktury2)-PopSloupec
### prevazene odhady uhrnu populacni sumy
sum(odh.struktury2)
sum(odh.struktury1)


