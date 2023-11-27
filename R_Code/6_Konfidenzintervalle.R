# Pakete
library(dplyr)
library(tidyr)

# Daten einlesen
setwd("H:/Kurs/Daten") #Speicherort
Dx_Ex <- read.table("Dx_Ex_Daten.txt")

# Konfidenzintervalle für SDRs und Lebenserwartung berechnen
# Dafür kann das PHEindicatormethods Paket verwendet werden

library(PHEindicatormethods)
# "Gewichte" wird in Script 3 erstellt
EU_standard <- Gewichte$EU

SDRs_CI <- Dx_Ex %>% group_by(PopName, Year, Sex) %>%
  phe_dsr(x = Deaths, n = Ex, stdpop = EU_standard)

# Gleiche SDR (Objekt "SDRs" wird in Script 3 erstellt)
filter(SDRs_CI, PopName=="Hessen" & Sex=="f" & Year==2022)$value
filter(SDRs, PopName=="Hessen" & Sex=="f" & Year==2022)$SDR_EU
# Aber nun mit CI
filter(SDRs_CI, PopName=="Hessen" & Sex=="f" & Year==2022)$lowercl
filter(SDRs_CI, PopName=="Hessen" & Sex=="f" & Year==2022)$uppercl

# Wir können CIs auch für die Lebenserwartung berechnen,
# aber die Funktion benötigt leider Daten in bestimmten Altersgruppen
# 0, 1-4, 5-9, 10-14...90+

# Deswegen müssen wir unsere Daten angleichen, indem wir umverteilen

split.Dx_Ex <- function(X) {
  
  X <- arrange(X, AgeGroup)
  
  Dx1to19 <- filter(X, AgeGroup==1)$Deaths
  Ex1to19 <- filter(X, AgeGroup==1)$Ex
  
  Dx20to29 <- filter(X, AgeGroup==20)$Deaths
  Ex20to29 <- filter(X, AgeGroup==20)$Ex
  
  split.Dx <- Dx1to19/4
  replace.Dx <- rep(split.Dx, 4)
  
  split.Dx2 <- Dx20to29/2
  replace.Dx2 <- rep(split.Dx2, 2)
  
  split.Ex <- Ex1to19/4
  replace.Ex <- rep(split.Ex, 4)
  
  split.Ex2 <- Ex20to29/2
  replace.Ex2 <- rep(split.Ex2, 2)
  
  out <- data.frame(Age = c(0, 1, seq(5, 90, 5)),
                    
                    Dx = c(filter(X, AgeGroup==0)$Deaths,
                           replace.Dx, replace.Dx2,
                           filter(X, AgeGroup>=30)$Deaths),
                    
                    Ex = c(filter(X, AgeGroup==0)$Ex,
                           replace.Ex, replace.Ex2,
                           filter(X, AgeGroup>=30)$Ex)
  )
  test.Dx <- !(sum(out$Dx) == sum(X$Deaths))
  test.Ex <- !(sum(out$Ex) == sum(X$Ex))
  
  if (test.Dx) {
    warning("Deaths do not sum up!")
  }
  
  if (test.Ex) {
    warning("Population does not sum up!")
  }
  
  return(out)
}

# An alle Kombination anwenden
Dx_Ex_umverteilt <- Dx_Ex %>% group_by(PopName, Year, Sex) %>%
  group_modify(~split.Dx_Ex(.))

# Voilá, die Altersgruppen passen nun
unique(Dx_Ex_umverteilt$Age)

# Die Summen stimmen
sum(Dx_Ex_umverteilt$Dx)==sum(Dx_Ex$Deaths)
sum(Dx_Ex_umverteilt$Ex)==sum(Dx_Ex$Ex)

# Nun können wir die Funktion anwenden
LE_CI <- Dx_Ex_umverteilt %>% group_by(PopName, Year, Sex) %>%
  phe_life_expectancy(Dx, Ex, Age)

# Obwohl wir etwas umverteilt haben, sind die e0 Werte seeeehr ähnlich
# (Objekt "LT.out" wird in Script 4 erstellt)
filter(LE_CI, PopName=="Hessen" & Sex=="m" & Year==2022 & Age==0)$value
filter(LT.out, PopName=="Hessen" & Sex=="m" & Year==2022 & x==0)$ex

# Recht kleine CI, aber für das Saarland oder Bremen größer
filter(LE_CI, PopName=="Hessen" & Sex=="m" & Year==2022 & Age==0)$lowercl
filter(LE_CI, PopName=="Hessen" & Sex=="m" & Year==2022 & Age==0)$uppercl

# Ende