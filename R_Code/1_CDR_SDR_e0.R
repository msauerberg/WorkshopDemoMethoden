## Daten einlesen
# setwd("H:/Kurs/Daten") hier den Ordner mit dem Datensatz eintragen
Daten <- read.table("SDR_Beispiel.txt")#Daten stammen von mortality.org
head(Daten)

# Summe der Todesfälle
sum(Daten$Dx_Deutschland)
sum(Daten$Dx_Schweiz)

# CDR, also Todesfälle per capita
CDR_DE <- sum(Daten$Dx_Deutschland) / sum(Daten$Nx_Deutschland)
CDR_CH <- sum(Daten$Dx_Schweiz) / sum(Daten$Nx_Schweiz)

round(CDR_DE, 4)
round(CDR_CH, 4)
Ratio_CDR <- CDR_DE/CDR_CH
round(Ratio_CDR, 2) #Deutlich höher in Deutschland

# Die ältere Altersstruktur in DE könnte einen Einfluss haben
C_DE <- Daten$Nx_Deutschland / sum(Daten$Nx_Deutschland)
C_CH <- Daten$Nx_Schweiz / sum(Daten$Nx_Schweiz)

# Einfacher plot
plot(1:21, C_DE, type="l", col="gold", ylab="Bevölkerungsanteil",
     xlab="Altersgruppe")
lines(1:21, C_CH, col="red")
legend("topleft", legend=c("DE", "CH"), fill=c("gold", "red"), bty="n")

# Sterberaten (mx)
mx_DE <- Daten$Dx_Deutschland / Daten$Nx_Deutschland
mx_CH <- Daten$Dx_Schweiz / Daten$Nx_Schweiz

# Sterberaten sind übers Alter hinweg höher in DE
plot(1:21, log(mx_DE), col="gold", type="l")
lines(1:21, log(mx_CH), col="red")

# CDR = Summe über mx * Cx
sum(mx_DE * C_DE)
CDR_DE

sum(mx_CH * C_CH)
CDR_CH

# Wir wollen deswegen C konstant halten, um einen besseren Vergleich von mx zu erhalten
# C entspricht hier der Standardbev. 2013
SDR_DE <- sum(mx_DE * Daten$Europa_Standard_2013)
SDR_CH <- sum(mx_CH * Daten$Europa_Standard_2013)

round(SDR_DE, 4)
round(SDR_CH, 4)
Ratio_SDR <- SDR_DE/SDR_CH
round(Ratio_SDR, 2)

# Wir könnten auch annehmen, beide Länder hätten die Altersstruktur von DE
SDR_DE <- sum(mx_DE * C_DE)
SDR_CH <- sum(mx_CH * C_DE)

round(SDR_DE, 4) #andere Werte
round(SDR_CH, 4) #andere Werte
Ratio_SDR <- SDR_DE/SDR_CH
round(Ratio_SDR, 2) #Unterschied gleich, das muss aber nicht immer so sein!


# Funktion einer Sterbetafel mit "mx" als Input
Sterbetafel <- function(mx){
  x <- c(0, 1, seq(5,95,5))
  m <- length(mx)
  n <- c(diff(x), NA)
  ax <- n/2; ax[1] <- 0.14;  ax[m] <- 1 / mx[m]
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

# Anwenden auf unsere Daten
Sterbetafel_DE <- Sterbetafel(mx = mx_DE)
Sterbetafel_CH <- Sterbetafel(mx = mx_CH)
head(Sterbetafel_DE)
head(Sterbetafel_CH)

# Lebenserwartung bei Geburt 2019, Deutschland vs. Schweiz
round(Sterbetafel_DE$ex[1], 2)
round(Sterbetafel_CH$ex[1], 2) #Höher in der Schweiz
Ratio_e0 <- round(Sterbetafel_CH$ex[1], 2) / round(Sterbetafel_DE$ex[1], 2)
round(Ratio_e0, 2) #Unterschied deutlich geringer verglichen mit SDR

# Vergleich der Überlebenskurven für Deutschland und Schweiz, 2019
plot(1:21, Sterbetafel_DE$lx, type="l", col="gold", xlab="Altersgruppe",
     ylab="Überlebende in der Sterbetafel")
lines(1:21, Sterbetafel_CH$lx, col="red")

# CDR und SDR in der Sterbetafel
CDR_Sterbetafel <- sum(Sterbetafel_DE$dx) / sum(Sterbetafel_DE$Lx) #Dx/Nx
C_Sterbetafel <- Sterbetafel_DE$Lx / sum(Sterbetafel_DE$Lx) #Altersstruktur
sum(mx_DE * C_Sterbetafel)
CDR_Sterbetafel #CDR in der Sterbetafel entspricht mx*Altersstruktur der Bev. in der Sterbetafel 

# Lebenserwartung ist nichts anderes als die Inverse
Sterbetafel_DE$ex[1]
1/CDR_Sterbetafel #Lebenserwartung kann also als CDR in der Sterbetafel verstanden werden

### Ende