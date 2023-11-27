
# Pakete
library(dplyr)
library(tidyr)

# Daten einlesen
setwd("H:/Kurs/Daten") #Speicherort
Dx_Ex <- read.table("Dx_Ex_Daten.txt")

# Sterberaten berechnen
Dx_Ex$mx <- Dx_Ex$Deaths / Dx_Ex$Ex

# Bev�lkerungsgewichte f�r altersstandardisierte Sterberaten (SDR)
# European Standard Population 2013
# https://ec.europa.eu/eurostat/web/products-manuals-and-guidelines/-/ks-ra-13-028
c1 <- c(0.01, sum(c(0.04, rep(0.055, 3))), sum(rep(0.06, 2)), 0.065, rep(0.07,4), 0.065,
        0.06, 0.055, 0.05, 0.04, 0.025, 0.015, 0.01)

# Unsere eigene berechnen f�r Deutschland im Jahr 2020
TotalPop <- aggregate(Ex ~ AgeGroup,
                      subset(Dx_Ex, select = -c(Deaths, Sex), Year==2020),
                      FUN = sum)

c2 <- c(TotalPop$Ex / sum(TotalPop$Ex))

# M�ssen 1 ergeben
sum(c1)
sum(c2)

# In einer Tabelle speichern
Gewichte <- data.frame(AgeGroup = unique(Dx_Ex$AgeGroup),
                       EU = c1,
                       DE = c2)

# Grafik zeigt die Unterschiede zwischen den beiden Bev�lkerungen
plot(Gewichte$AgeGroup, Gewichte$EU, col="blue", ylim=c(0, 0.2), axes=F,
     ylab = "Anteil der Bev�lkerung", xlab = "Altersgruppe", type="l")
lines(Gewichte$AgeGroup, Gewichte$DE, col="red")
axis(1, Gewichte$AgeGroup)
axis(2, seq(0, 0.2, 0.02))
legend(50, 0.16, legend=c("Europa 2013", "Deutschland 2020"), 
       fill = c("blue", "red"), bty="n")

# Zusammenf�hren aber diesmal mit "left_join()" anstatt "merge()"
Dx_Ex <- Dx_Ex %>% left_join(Gewichte)

# SDRs berechnen
SDRs <- Dx_Ex %>% group_by(PopName, Year, Sex) %>%
  summarise(SDR_EU = sum(c(mx * EU)) * 100000,
            SDR_DE = sum(c(mx * DE)) * 100000) %>%
  ungroup()

# Sortieren
SDRs <- arrange(SDRs, PopName, Sex, Year)

# Base R Grafik
par(mfrow=c(2,2), mar = c(4, 4, 2, 1)) # 2x2 plot mit angepasster Randgr��e
plot(2018:2022, subset(SDRs, PopName=="Hessen" & Sex=="f")$SDR_EU,
     col="orangered", type="l", ylim=c(800, 1200),
     xlab="Year", ylab="Standardized Death Rate", main="Hessen")
lines(2018:2022, subset(SDRs, PopName=="Hessen" & Sex=="f")$SDR_DE,
      col="orangered", lty=2)
legend(2018, 1150, legend=c("Standardbev�lkerung Europa 2013",
                            "Standardbev�lkerung Deutschland 2020"),
       lty=c(1,2), bty="n", cex=0.8)
legend(2020, 950, legend=c("Frauen", "M�nner"),
       fill=c("orangered", "skyblue"), bty="n", cex=0.8)
plot(2018:2022, subset(SDRs, PopName=="RheinlandPfalz" & Sex=="f")$SDR_EU,
     col="orangered", type="l", ylim=c(800, 1200),
     xlab="Year", ylab="", main="Rheinland Pfalz")
lines(2018:2022, subset(SDRs, PopName=="RheinlandPfalz" & Sex=="f")$SDR_DE,
      col="orangered", lty=2)

plot(2018:2022, subset(SDRs, PopName=="Hessen" & Sex=="m")$SDR_EU,
     col="skyblue", type="l", ylim=c(1000, 1600),
     xlab="Year", ylab="Standardized Death Rate", main="")
lines(2018:2022, subset(SDRs, PopName=="Hessen" & Sex=="m")$SDR_DE,
      col="skyblue", lty=2)

plot(2018:2022, subset(SDRs, PopName=="RheinlandPfalz" & Sex=="m")$SDR_EU,
     col="skyblue", type="l", ylim=c(1000, 1600),
     xlab="Year", ylab="", main="")
lines(2018:2022, subset(SDRs, PopName=="RheinlandPfalz" & Sex=="m")$SDR_DE,
      col="skyblue", lty=2)


# Diesselbe Grafik mit ggplot2
library(ggplot2)

# Vom wide format ins long format
SDRs_long <- pivot_longer(SDRs, col=4:5, names_to = "Standardbev�lkerung",
                          values_to = c("SDR"))

# Sortieren
SDRs_long <- arrange(SDRs_long, PopName, Standardbev�lkerung, Sex, Year)

sex_names <- c("f" = "Frauen",
               "m" = "M�nner")

pop_names <- c("RheinlandPfalz" = "Rheinland Pfalz",
               "Hessen" = "Hessen")

SDRs_long$PopName <- factor(SDRs_long$PopName, levels = c("Hessen", "RheinlandPfalz"))

gg <- SDRs_long %>% filter(PopName %in% c("Hessen", "RheinlandPfalz")) %>%
  ggplot(aes(x = Year, y = SDR, fill = Standardbev�lkerung, col=Sex)) +
  geom_line(aes(linetype=Standardbev�lkerung)) + 
  scale_color_manual(values=c("orangered", "skyblue")) +
  facet_wrap(~Sex*PopName,
             labeller = labeller(Sex = as_labeller(sex_names),
                                 PopName = as_labeller(pop_names),
                                 .multi_line = FALSE)) +
  theme_bw() +
  theme(legend.position="bottom")
gg  

# Es w�re interessant zu sehen, welches Bundesland die gr��ten bzw. geringsten 
# Verluste in der Sterblichkeitsrate zwischen den Jahren 2019 und 2021 zeigen
# H�ngt die Rangfolge von der gew�hlten Standardbev�lkerung ab?

# Long format zu wide format
wide_SDR <- pivot_wider(SDRs, names_from = c("Year", "Sex"),
                        values_from = c("SDR_EU", "SDR_DE"))

# Neuer dataframe names Ratio, der gef�llt werden soll
Ratios <- data.frame(PopName = wide_SDR$PopName)

# Relative Unterschiede zwischen 2019 und 2021
Ratios$SDR_EU_f <- wide_SDR$SDR_EU_2021_f / wide_SDR$SDR_EU_2019_f
Ratios$SDR_DE_f <- wide_SDR$SDR_DE_2021_f / wide_SDR$SDR_DE_2019_f

Ratios$SDR_EU_m <- wide_SDR$SDR_EU_2021_m / wide_SDR$SDR_EU_2019_m
Ratios$SDR_DE_m <- wide_SDR$SDR_DE_2021_m / wide_SDR$SDR_DE_2019_m

# Beispiel f�r die Rangfolge von SDR Differenzen f�r Frauen
# mit Europa als Standardbev�lkerung
View(arrange(Ratios, desc(SDR_EU_f)))

# Rankings f�r Frauen (Namen der Bundesl�nder sortiert nach H�he der Differenz)
rankings.SDR.EU.f <- arrange(Ratios, desc(SDR_EU_f))$PopName
rankings.SDR.DE.f <- arrange(Ratios, desc(SDR_DE_f))$PopName

# Ist die Rangfolge diesselbe?
rankings.SDR.EU.f == rankings.SDR.DE.f
# Finde die F�lle mit unterschiedlichen R�ngen
f�lle <- !(rankings.SDR.EU.f == rankings.SDR.DE.f)
rankings.SDR.EU.f[f�lle] #Beispiel: MV zeigt eine gr��ere Differenz als Bayern
rankings.SDR.DE.f[f�lle] #Hier ist die Differenz in Bayern gr��er als in MV
# --> In wenigen F�llen ist die Rangfolge anders 

### M�nner
rankings.SDR.EU.m <- arrange(Ratios, desc(SDR_EU_m))$PopName
rankings.SDR.DE.m <- arrange(Ratios, desc(SDR_DE_m))$PopName

rankings.SDR.EU.m == rankings.SDR.DE.m
f�lle <- !(rankings.SDR.EU.m == rankings.SDR.DE.m)
rankings.SDR.EU.m[f�lle]
rankings.SDR.DE.m[f�lle]
# Unterschiede in der Rangfolge geringer als bei den Frauen