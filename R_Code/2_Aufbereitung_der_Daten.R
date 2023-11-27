
# Daten einlesen
setwd("H:/Kurs/Daten") #Speicherort
dir() #Zeigt die Dateien im Ordner an

# Sterbefälle
Dx <- read.table("Data_BL_Deaths.txt", header=TRUE, sep=" ")

# Bevölkerung
Nx <- read.table("Data_BL_Population.txt", header=TRUE, sep=" ")

# Daten zeigen unterschiedliche Altersintervalle
unique(Dx$AgeGroup)
unique(Nx$AgeGroup)

# Bevölkerungsdaten aggregieren
Ziel.interval <- unique(Dx$AgeGroup) #Altersgruppen der Sterbefälle

# Gruppen erstellen
Nx$AgeGroupNew <- cut(Nx$AgeGroup, c(Ziel.interval, Inf),
                      right=FALSE, labels = Ziel.interval)

# Aggregieren basierend auf den Gruppen
NxGroups <- aggregate(Population ~ PopName + Year + Sex + AgeGroupNew,
                      data = subset(Nx, select = -AgeGroup), FUN = sum)

# Summen prüfen
sum(NxGroups$Population)
sum(Nx$Population)

# Alter numerisch definieren
unique(NxGroups$AgeGroupNew)#Alter is eine "factor variable"
NxGroups$AgeGroupNew <- as.numeric(as.character(NxGroups$AgeGroupNew))
unique(NxGroups$AgeGroupNew)#Nun ist sie numerisch

# Spaltenname anpassen
colnames(NxGroups)#Der Spaltenname sollte geändert werden
colnames(NxGroups)[colnames(NxGroups)=="AgeGroupNew"] <- "AgeGroup"
colnames(NxGroups)#Nun stimmt der Name

#Wir benötigen den mittleren Bevölkerungsstand
#Momentan beziehen sich die Daten auf den 31.12.XXXX
#Die Bevölkerung in der Mitte des Jahres 2018
#ist (Bev am 01.01.2018 + Bev am 01.01.2019) / 2

# Wir nehmen an, dass der Bevölkerungsstand am 31.12.XXXX
# mit dem am 01.01.XXXX+1 übereinstimmt (also ein Tag später)
NxGroups$Year <- NxGroups$Year + 1

# Daten können sehr einfach mit dem "dplyr" und "tidyr"
# Paket bearbeitet werden
# Eine beliebte Alternative ist das "data.table" Paket

library(dplyr)
library(tidyr)

Ex <- NxGroups %>% group_by(PopName, Sex, AgeGroup) %>%
  mutate(Ex = c(Population + lead(Population)) / 2) %>%
  ungroup()

# Test
c(subset(Ex, PopName=="Hessen" & Sex=="f" & Year==2018 & AgeGroup==30)$Population+
    subset(Ex, PopName=="Hessen" & Sex=="f" & Year==2019 & AgeGroup==30)$Population)/2

subset(Ex, PopName=="Hessen" & Sex=="f" & Year==2018 & AgeGroup==30)$Ex
# Die Zahlen stimmen überein

# Unnötige Spalten und Reihen löschen
Ex <- subset(Ex, select = -Population, Year < 2023)

# Sortieren
Ex <- arrange(Ex, PopName, Year, Sex, AgeGroup)

# Zusammenführen der Daten
Dx_Ex <- merge(Dx, Ex, by=c("PopName", "Year", "Sex", "AgeGroup"),
               all = TRUE)

# Speichern
write.table(Dx_Ex, "Dx_Ex_Daten.txt")

# Ende