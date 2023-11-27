# Daten einlesen
setwd("H:/Kurs/Daten") #Speicherort
e0.wide <- read.table("e0_Daten.txt")

### Werte der Lebenserwartung im Jahr 2019 in eine Karte einzeichnen
library(sf)
library(cowplot)
library(ggpubr)

setwd("H:/Kurs/Shapefile")
shape <- read_sf("NUTS_RG_01M_2021_3035.shp") #Von eurostat.eu

# Wir benötigen lediglich DE auf Bundeslandebene
shape2 <- filter(shape, LEVL_CODE==1 & CNTR_CODE=="DE")

# Wir wollen unsere Lebenserwartungen hinzufügen
e0.merge <- select(e0.wide, c("PopName", "X2019_f", "X2019_m"))
# Spalten umbennen
colnames(e0.merge)[c(2:3)] <- c("e0_F", "e0_M")

# Schlüssel, damit wir beide Datensätze zusammenfügen können
e0.merge$LEVL_CODE <- 1
# Das sind die NUTS codes für die Bundesländer in Deutschland
e0.merge$NUTS_ID <- c(paste("DE", 1:9, sep=""),
                      paste("DE", LETTERS[c(1:7)], sep=""))

# Zusammenführen
shape3 <- shape2 %>% left_join(e0.merge)

# Erste Karte mit stetiger Farbskala, getrennt nach Geschlecht

maps_conti.f <- shape3 %>% 
  ggplot()+
  geom_sf(aes(fill = e0_F), color = NA)+
  scale_fill_viridis_c(option = "B", direction=-1)+
  coord_sf(datum = NA)+
  ggtitle("Frauen, Lebenserwartung bei Geburt")+
  theme_map()+
  theme(legend.position = "bottom")+
  labs(fill = "e0 (in years)")

maps_conti.m <- shape3 %>% 
  ggplot()+
  geom_sf(aes(fill = e0_M), color = NA)+
  scale_fill_viridis_c(option = "B", direction=-1)+
  coord_sf(datum = NA)+
  ggtitle("Männer, Lebenserwartung bei Geburt")+
  theme_map()+
  theme(legend.position = "bottom")+
  labs(fill = "e0 (in years)")

# Beide Karten in einer Grafik
ggarrange(maps_conti.f, maps_conti.m)

# Nun mit diskreter Farbskala und nur einer Legende

# Dazu brauchen wir die Daten im long format
shape4 <- pivot_longer(shape3, col=12:13 ,names_to = "e0", values_to = "value")

# So ändern wir die Beschriftung in der Grafik
facet.labs <- c("Frauen","Männer")
names(facet.labs) <- c("e0_F","e0_M")

maps_discrete <- 
  ggplot(shape4)+
  geom_sf(aes(fill = cut(value,quantile(value,probs=c(seq(0,1,0.1)),na.rm=T),include.lowest = T)),
          color = NA)+
  scale_fill_viridis_d(option = "B",name="Lebenserwartung bei Geburt \n", direction=-1)+
  coord_sf(datum = NA)+facet_wrap(~e0, labeller = labeller(e0 = facet.labs), nrow=1) +
  theme_map()+
  theme(legend.position = "bottom",legend.title = element_text(hjust = 0.5,size=12),
        legend.box = "horizontal",
        legend.justification = "center",legend.text=element_text(size=10))+
  labs(fill = "e0 (in years)") + guides(fill=guide_legend(title.position="top"))

maps_discrete

# Ende