
# Pakete
library(dplyr)
library(tidyr)

# Daten einlesen
setwd("H:/Kurs/Daten") #Speicherort
Dx_Ex <- read.table("Dx_Ex_Daten.txt")

# Sterberaten berechnen
Dx_Ex$mx <- Dx_Ex$Deaths / Dx_Ex$Ex

# Neben der SDR, können wir Lebenserwartungen berechnen und diese
# Werte als altersstandardisierten Mortalitätsindikator verwenden  

# Diese Funktion kommt von Giancarlo Camarda
# https://sites.google.com/site/carlogiovannicamarda/r-stuff

life_table <- function(x, Nx, Dx, sex="M", ax=NULL){
  m <- length(x)
  mx  <- Dx/Nx
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

# Diese Funktion wollen wir auf alle Kombination (Geschlecht*Jahr*Bundesland)
# anwenden. Wir berechnen also insgesamt 160 (2 * 5 * 16 = 160) Sterbetafeln 

LE.apply <- function(X) {
  
  sort.X <- arrange(X, AgeGroup)
  Age <- X$AgeGroup
  Dx <- X$Deaths
  Ex <- X$Ex
  # andere Annahmen über "ax" je nach Geschlecht
  if (any(X$Geschlecht=="Mann")) {
    
    LT <- life_table(x = Age, Dx = Dx, Nx = Ex, sex = "M")
    
  }
  
  if (any(X$Geschlecht=="Frau")) {
    
    LT <- life_table(x = Age, Dx = Dx, Nx = Ex, sex = "F")
    
  }
  
  return(LT)
  
}

# Neue Variable für das Geschlecht
Dx_Ex$Geschlecht <- NA
Dx_Ex$Geschlecht[Dx_Ex$Sex=="m"] <- "Mann"
Dx_Ex$Geschlecht[Dx_Ex$Sex=="f"] <- "Frau"

# Funktion anwenden
LT.out <- Dx_Ex %>% group_by(PopName, Year, Sex) %>%
  group_modify(~LE.apply(.))

# Wie viele Altersgruppen?
length(unique(LT.out$x)) #16

# Also muss LT.out 16 * 160 Reihen besitzen
16 * 160
nrow(LT.out)

# Als Excel Tabelle speichern mit verschiedenen "Sheets"
library(openxlsx)

# Zerlege den dataframe nach Bundesland und Geschlecht
split_by_State_and_Sex <- split(LT.out, ~ PopName + Sex)

# Abspeichern
setwd("H:/Kurs/Output")
write.xlsx(split_by_State_and_Sex, "LifeTables_Bundesland_2018bis2022.xlsx")

# Welches Bundesland hatte den größten Rückgang in der Lebenserwartung
# zwischen 2019 und 2021?

# Wähle die entsprechenden Jahre und Spalten
e0.select <- select(subset(LT.out, x==0 & Year %in% c(2019, 2021)),
                    c(PopName, Year, Sex, ex))

# Wieder in wide format
e0.wide <- pivot_wider(e0.select, names_from = c("Year","Sex"), values_from = "ex")

# Absolute unterschiede in der Lebenserwartung
e0.wide$Diff.f <- e0.wide$`2021_f` - e0.wide$`2019_f`
e0.wide$Diff.m <- e0.wide$`2021_m` - e0.wide$`2019_m`

# Speichern
write.table(e0.wide, "e0_Daten.txt")

# Rankings? Sortiere von der größten negativen Zahl zur kleinsten
e0.ranking.f <- arrange(e0.wide, Diff.f)$PopName
e0.ranking.m <- arrange(e0.wide, Diff.m)$PopName

# Alles in eine Tabelle (funkioniert nur wenn vorher Script 3 ausgeführt wurde)
ranking.frame <- data.frame(Rank = 1:16,
                            Frauen_SDR_DE = rankings.SDR.DE.f,
                            Frauen_SDR_EU = rankings.SDR.EU.f,
                            Frauen_e0 = e0.ranking.f,
                            Männer_SDR_DE = rankings.SDR.DE.m,
                            Männer_SDR_EU = rankings.SDR.EU.m,
                            Männer_e0 = e0.ranking.m)

View(ranking.frame)

# Ende