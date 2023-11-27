
# Pakete
library(dplyr)
library(tidyr)

# Daten einlesen
setwd("H:/Kurs/Daten") #Speicherort
Dx_Ex <- read.table("Dx_Ex_Daten.txt")

# Sterberaten berechnen
Dx_Ex$mx <- Dx_Ex$Deaths / Dx_Ex$Ex

# Altersdekomposition der Lebenserwartung bei Geburt

# Im Prinzip dieselbe Funktion aber nun mit mx als Input und nur 
# Lebenserwartung im Alter 0 als Output

lifetable.mx <- function(x, mx, sex="M", ax=NULL){
  m <- length(x)
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
  return.df <- data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df$ex[1])
}

# Das Paket beinhaltet die Funktion für unsere Altersdekomposition
library(DemoDecomp)

# leere Liste, die wir mit Werten füllen wollen
list_of_datasets <- list()

for (i in 1:length(unique(Dx_Ex$PopName))) {
  
  # loop over Bundesländer
  region <- unique(Dx_Ex$PopName)[i]
  
  # Nehme die Sterberaten für jedes Bundesland, getrennt nach Geschlecht und Jahr
  mx.2019.m <- arrange(filter(Dx_Ex, PopName==region & Sex=="m" & Year==2019), AgeGroup)$mx
  mx.2022.m <- arrange(filter(Dx_Ex, PopName==region & Sex=="m" & Year==2022), AgeGroup)$mx
  
  mx.2019.f <- arrange(filter(Dx_Ex, PopName==region & Sex=="f" & Year==2019), AgeGroup)$mx
  mx.2022.f <- arrange(filter(Dx_Ex, PopName==region & Sex=="f" & Year==2022), AgeGroup)$mx
  
  the.age <- unique(Dx_Ex$AgeGroup)
  
  decompo.2022.m <- stepwise_replacement(func=lifetable.mx, pars1 = mx.2019.m,
                                         pars2 = mx.2022.m, sex="M", x=the.age)
  
  decompo.2022.f <- stepwise_replacement(func=lifetable.mx, pars1 = mx.2019.f,
                                         pars2 = mx.2022.f, sex="F", x=the.age)
  
  out <- data.frame(Age = the.age,
                    PopName = region,
                    Beitrag_M = decompo.2022.m,
                    Beitrag_F = decompo.2022.f)
                    
  
  list_of_datasets[[i]] <- out
  
}

Ergebnisse <- do.call("rbind", list_of_datasets)

# Beispiel: Die Summe der Altersbeitraege stimmt mit der Differenz der
# Lebenserwartung zwischen den Jahren 2019 und 2022 überein

sum(filter(Ergebnisse, PopName=="Hessen")$Beitrag_F) #-0.6947122
# "LT.out" wurde in Script 4 erstellt
filter(LT.out, PopName=="Hessen" & Year==2022 & Sex=="f" & x==0)$ex -
  filter(LT.out, PopName=="Hessen" & Year==2019 & Sex=="f" & x==0)$ex
# ebenfalls -0.6947122

# Das Balkendiagramm zeigt, dass vor allem die hohen Altersgruppen zu der
# Differenz in der Lebenserwartung beigetragen haben
b <- barplot(filter(Ergebnisse, PopName=="Hessen")$Beitrag_F)
axis(1, at = b, labels = filter(Ergebnisse, PopName=="Hessen")$Age)

# Ende