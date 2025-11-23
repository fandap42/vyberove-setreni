library(ggplot2)
library(sampling)

mzdy <- read.csv("Mzdy.txt")
mzdy$vzdel <- as.factor(mzdy$vzdel)
# mzdy[order(mzdy$vzdel), ] # ta data už vypadaji serazena
mm <- 9
n <- 50 + abs(mm - 6) * 10
N <- nrow(mzdy)
true_mean <- mean(mzdy$Mzda)
true_mean_strat <- c(mean(mzdy$Mzda[mzdy$vzdel==1]), mean(mzdy$Mzda[mzdy$vzdel==2]), mean(mzdy$Mzda[mzdy$vzdel==3]), mean(mzdy$Mzda[mzdy$vzdel==4]))

# Proveďte rozklad rozptylu mezd, posuďte podíl vnitroskupinové a meziskupinové variability mezd.
anova <- anova(aov(Mzda ~ vzdel, data=mzdy))

summary(anova)
paste("Většina variability je vysvětlená vzděláním :)")

ggplot(data=mzdy, aes(x=vzdel, y=Mzda, fill=vzdel)) +
  geom_boxplot()

# Proveďte alokaci výběru:
## a. rovnoměrnou <- v kazde skupine n/pocet skupin pozorovani
rov_aloc <- c(n/4, n/4, n/4, n/4)
r_strata <- strata(mzdy, stratanames = c("vzdel"), size=rov_aloc, method="srswor", description = T)
## b. proporcionální <- podle pomeru kolik je v jednotlivych skupinach pozorovani
popsize <- table(mzdy$vzdel)
prop_aloc <- pmin(pmax(round((n*popsize/sum(popsize))),2),popsize)
p_strata <- strata(mzdy, stratanames = c("vzdel"), size=prop_aloc, method="srswor", description = T)
## c. optimální <- v prezentaci cviceni 4
popvar <- c(sd(mzdy$Mzda[mzdy$vzdel==1]), sd(mzdy$Mzda[mzdy$vzdel==2]), sd(mzdy$Mzda[mzdy$vzdel==3]), sd(mzdy$Mzda[mzdy$vzdel==4]))
opt_aloc <- n * (popsize*popvar / sum(popsize*popvar))
opt_aloc <- round(opt_aloc)
sum(opt_aloc) # je to dobry, je to porad 80
o_strata <- strata(mzdy, stratanames = c("vzdel"), size=opt_aloc, method="srswor", description = T)

# Pro 95% spolehlivost určete interval spolehlivosti odhadu průměrné mzdy pro prostý náhodný výběr bez vracení 
# z celé populace a pro stratifikované náhodné výběry z bodu 2. Povede využití stratifikovaného výběru k zvýšení
# přesnosti odhadu průměrné mzdy? Tratíme hodně na přesnosti, když použijeme proporcionální alokaci místo optimální? 
# Porovnejte přesnost výběrových plánů mezi sebou.

# 95% CI PNV z celé populace
pnv_sample <- sample(mzdy$Mzda, size = n)

odh_var <- (1-n/N)*var(pnv_sample)/n
odh_prum <- mean(pnv_sample)
CI <- c(odh_prum + qt(0.025,n-1)*sqrt(odh_var),
        odh_prum - qt(0.025,n-1)*sqrt(odh_var))
cat("Odhad prumerne mzdy s pouzitim PNV cini: ", round(odh_prum))
cat("95% interval spolehlivosti prumerne mzdy s pouzitim PNV cini: ", round(CI))
dplyr::between(true_mean, CI[1], CI[2])

# 95% CI ze strat
r_sample <- getdata(mzdy, r_strata)
p_sample <- getdata(mzdy, p_strata)
o_sample <- getdata(mzdy, o_strata)

get_CI <- function(sample, aloc, name) {
  vyb_prum_strat <- c(mean(sample$Mzda[sample$vzdel==1]),
                      mean(sample$Mzda[sample$vzdel==2]), 
                      mean(sample$Mzda[sample$vzdel==3]),
                      mean(sample$Mzda[sample$vzdel==4]))
  vyb_var_strat <- c(var(sample$Mzda[sample$vzdel==1]), 
                     var(sample$Mzda[sample$vzdel==2]), 
                     var(sample$Mzda[sample$vzdel==3]), 
                     var(sample$Mzda[sample$vzdel==4]))
  odh_var_strat <- c((1-aloc/popsize)*vyb_var_strat/aloc)
  odh_prum <- sum(popsize*vyb_prum_strat)/N
  cat("Odhad prumerne mzdy s pouzitim", name, "alokace strat cini: ", round(odh_prum))
  
  odh_var <- sum((popsize^2)*odh_var_strat)/N^2
  CI <- c(odh_prum+qt(0.025,n-1)*sqrt(odh_var),
          odh_prum-qt(0.025,n-1)*sqrt(odh_var))
  
  cat("95% interval spolehlivosti prumerne mzdy s pouzitim", name, "alokace strat cini: ", round(CI))
  dplyr::between(true_mean, CI[1], CI[2])
  cat("Sirka intervalu je", CI[2]-CI[1])
  
  # Pro oblastní výběry dle ii) spočítejte pro 95% spolehlivost rovněž intervaly spolehlivosti pro jednotlivá strata. 
  CI_strat_h<-vyb_prum_strat-qt(0.025,aloc-1)*sqrt(vyb_var_strat)
  CI_strat_d<-vyb_prum_strat+qt(0.025,aloc-1)*sqrt(vyb_var_strat)
  cat("95% interval spolehlivosti prumerne mzdy s pouzitim", name, "alokace strat cini: ", round(CI))
  cat("Sirky intervalu pro jednotliva strata", CI_strat_h - CI_strat_d)
  tab <- cbind(c("základní škola", "střední škola bez maturity", "střední škola s maturitou", "vysoká škola"),
               round(CI_strat_d), 
               round(CI_strat_h),
               round(CI_strat_h - CI_strat_d),
  # Dávají dostatečně přesné odhady průměrných příjmů jednotlivých skupin (zadavatel považuje odhad průměrné mzdy 
  # za dostatečně přesný, když šířka intervalu spolehlivosti 2Δ nepřesáhne 10 % populačního průměru). 
               round(CI_strat_h - CI_strat_d) <= true_mean/10, # ?? je takhle myšleno 10 % populačního průměru??
               round(true_mean_strat))
  
  knitr::kable(tab, col.names = c("stratum - nejvyšší dosažené vzdělání", "dolní hranice CI", "horní hranice CI", "šířka CI", "dostatečná přesnost", "populační průměr strata"),
               caption = paste("95% konfidenční intervaly jednotlivých strat s použitím", name, "alokace"))
  return(vyb_prum_strat)
}

tab <- data.frame(strata=rep(c("základní škola", "střední škola bez maturity", "střední škola s maturitou", "vysoká škola"), 3))

tab["alokace"] <- c(rep("rovnomerna", 4), rep("proporcni", 4), rep("optimalni", 4))
tab["prumeru"] <- c(get_CI(r_sample, rov_aloc, "rovnomerne"), get_CI(p_sample, prop_aloc, "proporcni"), get_CI(o_sample, opt_aloc, "optimalni"))
tab["populacni"] <- c(rep(true_mean_strat, 3))


# Která z alokací výběru je podle vás vhodná, pokud je cílem výběrového šetření získat při daném rozsahu
# výběru n co nejkvalitnější odhady průměrných mezd jednotlivých skupin zaměstnanců? (3b) 

ggplot(data = tab) +
  geom_point(aes(x=strata, y=prumer, col=alokace), size=3) +
  geom_point(aes(x=strata, y=populacni, col="populační průměr"), shape=4, size=3) +
  labs(title="Srovnání odhadů průměrů a populačních hodnot") +
  xlab("Stratum") +
  ylab("Průměrná mzda") +
  scale_colour_viridis_d()



