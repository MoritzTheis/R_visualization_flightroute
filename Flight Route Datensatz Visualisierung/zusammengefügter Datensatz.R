#libraries für Karten
library(ggplot2)
library(ggmap)
library(maptools)
library(osmdata)
library(sf)
library(dplyr)
library(rnaturalearth) 
library(rnaturalearthdata)
library(rgeos)
library(colorspace)


daten <- read.csv(file=file.choose(), encoding = "UTF-8")



#GLOBALE FLUGROUTEN VON AIRLINES

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(fill="black", colour = "white")+
  theme_void()+
  
geom_point(data=daten, 
             mapping = aes(y=source.Lattitude, x=source.Longitude), 
             size= 0.002, 
             colour = "orange"
  )+
  geom_point(data=daten, 
             mapping = aes(y=destination.Lattitude, x=destination.Longitude), 
             size= 0.002, 
             colour = "orange"
  )+
  geom_path(data=daten[daten$source.Country == "China",], #wie vernetze ich source und destination coords
            mapping = aes(x =destination.Longitude, y=destination.Lattitude, group=airline.ID),
            size = 0.05,
            colour = "red",
  )+
  labs(title = "Globale Routen",
)










#ROUTEN INNERHALB VON DEUTSCHLAND (mehr Striche = mehrere Airlines fliegen diese Route)


germany <- ne_countries(country = "germany", scale = "medium", returnclass = "sf")

countsabflüge_airport <- daten[daten$source.Country == "Germany",] %>%
  group_by(source.Name, source.Lattitude, source.Longitude) %>%
  summarize(Abflüge = n())

Germany <- daten[daten$source.Country == "Germany",] 

Germany_1 <- Germany[Germany$destination.Country == "Germany",] 
  
inlandverbindungen <- Germany_1 %>%
  group_by(source.Name, source.Lattitude, source.Longitude, destination.Longitude, destination.Lattitude)  %>%
  summarize(Verbindungen = n())

Germany_1 <- left_join(Germany_1, inlandverbindungen, by = c("source.Name"))
 
 
ggplot(data=germany) +
  geom_sf(fill = "black", colour = "white")+
  coord_sf(xlim = c(5,15), ylim = c(47,55))+
  theme_void()+ #lon und lat ausblenden
  
  #Routenlinien einfügen
  geom_path(data=Germany_1, 
            mapping = aes(x = source.Longitude.x , y = source.Lattitude.x, size = Verbindungen),
            alpha = 0.15,
            colour = "red",
  )+
geom_point(data=countsabflüge_airport, 
           mapping = aes(y=source.Lattitude, x=source.Longitude, size = Abflüge),
           colour = "brown"
)+
  labs(title = "Routen in Deutschland",
       subtitle = "Flughäfen mit den meisten Inland-Verbindungen",
       caption = "Linienstärke = Anzahl an Airlines für diese Route",
       size = "Anzahl an 
ausgehenden 
Verbindungen"
  )+
  
  #relevanteste Flughafen in Deutschland
  #Frankfurt hat mehrere Flughafen (Hahn, FaM Airport)
  geom_text(data = Germany_1[Germany_1$source.Name == "Frankfurt am Main Airport",],
            mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = source.City),
            nudge_y = -0.2,
            colour = "grey"
  )+
  geom_text(data = Germany_1[Germany_1$source.City == "Munich",],
            mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "München"), #
            nudge_y = -0.2,
            colour = "grey"
  )+
  #In Berlin sind mehrere Flughäfen, deswegen source.Name
  geom_text(data = Germany_1[Germany_1$source.Name == "Berlin-Tegel Airport",],
            mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = source.City), 
            nudge_y = 0.3,
            colour = "grey"
  )+
  geom_text(data = Germany_1[Germany_1$source.City == "Duesseldorf",],
            mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "Düsseldorf"), 
            nudge_y = 0.2,
            colour = "grey"
  )+
  #In Hamburg sind auch mehrere Flughäfen
  geom_text(data = Germany_1[Germany_1$source.Name == "Hamburg Airport",],
            mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = source.City), 
            nudge_y = -0.2,
            colour = "grey"
)
            
#insgesamt 243 Routen in Deutschland















#ROUTEN VON DEUTSCHLAND INS AUSLAND

RoutenausD <- data.frame(daten[daten$source.Country == "Germany",])
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(fill = "grey", colour = "grey")+
  coord_sf(xlim = c(-120,130), ylim = c(-50,130))+
  theme_void()+
  
  geom_point(data=RoutenausD[RoutenausD$source.Country == "Germany",], 
             mapping = aes(y=source.Lattitude, x=source.Longitude), 
             size= 0.02, 
             colour = "orange"
  )+
  geom_path(data=RoutenausD,
            mapping = aes(x =destination.Longitude, y=destination.Lattitude, group=airline.ID),
            alpha = 0.3,
            colour = "red",
  )+
  labs(title = "Flugrouten aus Deutschland"
)














VERSUCH für Routenaus Deutschland in andere Länder nur in Europa
################################################



#Verbindungen von Deutschland ins Ausland (ausschließlich Europa)

#Datensatz erstellen für Reiseziele
Reiseziele <- data.frame(daten[daten$source.Country == "Germany",])
europe <- ne_countries(continent = "europe", scale = "medium", returnclass = "sf")


ggplot(data=europe) +
  geom_sf(fill = "grey")+
  coord_sf(xlim = c(-22,40), ylim = c(35,75))+
  theme_void()+ #lon und lat ausblenden
  
  #Datensatz Reiseziele von Deutschland
  
  
  #Airports in Europa ploten

geom_point(data=Reiseziele[Reiseziele$source.Country == "Germany",], 
             mapping = aes(y=source.Lattitude, x=source.Longitude), 
             size= 0.5, 
             colour = "blue",
  )+
  
  #Ziele von den Flugzeugen
  geom_point(data=Reiseziele[Reiseziele$destination.Country == "Spain",], 
             mapping = aes(y=destination.Lattitude, x=destination.Longitude), 
             size= 0.5, 
             colour = "red"
  )+
  geom_point(data=Reiseziele[Reiseziele$destination.Country == "Italy",], 
             mapping = aes(y=destination.Lattitude, x=destination.Longitude), 
             size= 0.5, 
             colour = "red"
  )+
  geom_point(data=Reiseziele[Reiseziele$destination.Country == "Greece",], 
             mapping = aes(y=destination.Lattitude, x=destination.Longitude), 
             size= 0.5, 
             colour = "red"
  )+
  

  #Routenverbindungen einfügen

  geom_path(data=Reiseziele[Reiseziele$destination.Country == "Spain",],
            mapping = aes(x =source.Longitude, y=source.Lattitude, group = airline.ID),
            alpha = 0.2,
            colour = "red",
            stat = "identity"
  )+
  labs(title = "Deutschland: Beliebte Reiseziele"
)
  







