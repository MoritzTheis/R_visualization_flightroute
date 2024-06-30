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

germany <- ne_countries(country = "germany", scale = "medium", returnclass = "sf")

countsabflüge_airport <- daten[daten$source.Country == "Germany",] %>%
  group_by(source.Name, source.Lattitude, source.Longitude) %>%
  summarize(Abflüge = n())

Germany <- daten[daten$source.Country == "Germany",] 

Germany_1 <- Germany[Germany$destination.Country == "Germany",] 

inlandverbindungen <- Germany_1 %>%
  group_by(source.Name, source.Lattitude, source.Longitude, destination.Longitude, destination.Lattitude)  %>%
  summarize(Verbindungen = n())

Germany_1 <- left_join(Germany_1, inlandverbindungen, by = c("source.Name","destination.Longitude","destination.Lattitude"))


ggplot(data=germany) +
  geom_sf(fill = "black", colour = "white")+
  coord_sf(xlim = c(5,15), ylim = c(47,55))+
  theme_void()+ #lon und lat ausblenden
  
  #Routenlinien einfügen
  geom_path(data=Germany_1, 
            mapping = aes(x = source.Longitude.x , y = source.Lattitude.x, size = Verbindungen),
            alpha = 0.3,
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
  )+
  geom_text(data = Germany_1[Germany_1$source.Name == "Stuttgart Airport",],
            mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = source.City), 
            nudge_y = -0.1,
            colour = "grey"
)

#insgesamt 243 Routen in Deutschland

