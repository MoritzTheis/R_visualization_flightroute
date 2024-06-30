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

Datensätze speichern
#write.csv(Daten, "C:\\Users\\ArneMorawitz\\Desktop\\fertiger_Datensatz.csv", row.names = FALSE )
#write.csv(Routen_aus_Deu, "C:\\Users\\ArneMorawitz\\Desktop\\Routen_aus_De.csv", row.names = FALSE )
#write.csv(Routen_aus_usa, "C:\\Users\\ArneMorawitz\\Desktop\\Routen_aus_USA.csv", row.names = FALSE )


#Routen_aus_Deu <- read.csv(file = file.choose(), encoding = "latin1")
#Routen_aus_usa <- read.csv(file = file.choose(), encoding = "latin1")
#Daten <- read.csv(file = file.choose(), encoding = "latin1")

daten <- read.csv(file=file.choose(), encoding = "UTF-8") #fertiger_Datensatz




Amerika <- daten[daten$source.Country == "United States of America",]

Amerika_1 <- Amerika[Amerika$destination.Country == "United States of America",]

americacountsabflüge_airport <- Amerika %>%
  group_by(source.Name, source.Lattitude, source.Longitude) %>%
  summarize(Abflüge = n())

americainlandverbindungen <- Amerika_1 %>%
  group_by(source.Name, source.Lattitude, source.Longitude, destination.Longitude, destination.Lattitude)  %>%
  summarize(Verbindungen = n())

Amerika_1 <- left_join(Amerika_1, americainlandverbindungen, by = c("source.Name","destination.Lattitude","destination.Longitude"))


#GLOBALE FLUGROUTEN VON AIRLINES
world <- ne_countries(scale = "medium", returnclass = "sf")
amerika <- ne_countries(country = "United States of America",scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf(fill = "black")+
  coord_sf(xlim = c(-170,-70), ylim = c(15,71))+
  theme_void()+
    
    #Routenlinien einfügen
    geom_path(data=Amerika_1[Amerika_1$ausgehende_routen >9,], 
            mapping = aes(x = source.Longitude.x , y = source.Lattitude.x, size = Verbindungen),
            alpha = 0.1,
            colour = "red"
    )+
    geom_point(data=americacountsabflüge_airport[americacountsabflüge_airport$Abflüge >9,], 
               mapping = aes(y=source.Lattitude, x=source.Longitude, size = Abflüge, colour = Abflüge)
    )+
    scale_size_binned(range = c(0.02, 4)
    )+
    scale_color_binned()+
  
    guides(
    colour = guide_bins("Abflüge"),
    size = guide_bins("Abflüge")
    )+
    
    labs(title = "Flugverbindungen in Amerika",
         subtitle = "Flughäfen mit den meisten Abflügen",
         caption = "Linienstärke = Anzahl an Airlines für diese Route"
    )+
    
  
    geom_text(data = Amerika_1[Amerika_1$source.Name == "Hartsfield Jackson Atlanta International Airport",],
              mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "Atlanta"),
              nudge_y = 0.7,
              colour = "grey"
    )+
    geom_text(data = Amerika_1[Amerika_1$source.Name == "Chicago O'Hare International Airport",],
              mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "Chicago"), 
              nudge_y = 0.7,
              colour = "grey"
    )+
    geom_text(data = Amerika_1[Amerika_1$source.Name == "Los Angeles International Airport",],
              mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "Los Angeles"), 
              nudge_y = 0.7,
              colour = "grey"
    )+
    geom_text(data = Amerika_1[Amerika_1$source.Name == "Dallas Fort Worth International Airport",],
              mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "Dallas"), 
              nudge_y = 0.7,
              colour = "grey"
    )+
    geom_text(data = Amerika_1[Amerika_1$source.Name == "John F Kennedy International Airport",],
              mapping = aes(y=source.Lattitude.x, x=source.Longitude.x, label = "New York"), 
              nudge_y = 0.7,
              colour = "grey"
)