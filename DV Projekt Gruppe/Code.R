library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(colorspace)
library(sf)
library(ggplot2)
library(maptools)
library(osmdata)
library(dplyr)

daten <- read.csv2(file = file.choose(), encoding =  "UTF-8")
world <- ne_countries(scale = "medium", returnclass = "sf")

#Hilfscode
deutsche_Abflüge <- daten[daten$source.Country == "Germany",]

count_deutsche_Zielflughäfen <- deutsche_Abflüge %>%
  group_by(destination.Name) %>%
  summarize(Zielflughäfen=n())

count_deutsche_Zielländer <- deutsche_Abflüge %>%
  group_by(destination.Name) %>%
  summarize(Zielverbingen=n())

deutsche_Abflüge <- left_join(deutsche_Abflüge, count_deutsche_Zielflughäfen, by=c("destination.Name"))
deutsche_Abflüge <- left_join(deutsche_Abflüge, count_deutsche_Zielländer, by=c("destination.Name"))

USA_Abflüge <- daten[daten$source.Country == "United States of America",]

count_USA_Zielflughäfen <- USA_Abflüge %>%
  group_by(source.Name) %>%
  summarize(Zielflughäfen=n())

count_USA_Zielländer <- USA_Abflüge %>%
  group_by(destination.Country) %>%
  summarize(Zielländer=n())

USA_Abflüge <- left_join(USA_Abflüge, count_USA_Zielflughäfen, by=c("source.Name"))
USA_Abflüge <- left_join(USA_Abflüge, count_USA_Zielländer, by=c("destination.Country"))

airlines_count <- daten %>%
  group_by(airline.name) %>%
  summarize(Anzahl_Verbindungen=n())

daten <- left_join(daten, airlines_count, by=c("airline.name"))

#Karten 

airports_count <- airports %>%
  group_by(Country)%>%
  mutate(Country=recode(Country, "United States"="United States of America")) %>%
  summarize(Airports_eines_Landes=n()) %>%
  rename(geounit = Country)
world <- left_join(world, airports_count)

#Anzahl an Flughäfen Weltweit                                  
ggplot(data = world) +
  geom_sf(mapping = aes(fill = Airports_eines_Landes)) +
  scale_fill_continuous_sequential(palette="Inferno",trans = "sqrt") +
  labs(title="Anzahl an Flughäfen", subtitle = "Weltweit pro Land",  fill="" ) +
  theme_void()

#Anzahl an Flughäfen Europa
ggplot(data = world) +
  geom_sf(mapping = aes(fill = Airports_eines_Landes)) +
  scale_fill_continuous_sequential(palette="Inferno",trans = "sqrt") +
  coord_sf(xlim = c(-20, 60), ylim = c(30, 80)) +
  labs(title="Anzahl an Flughäfen", subtitle = "Europa pro Land",  fill="" ) +
  theme_void()

#Charts

#Anzahl an Routen pro Stadt Deutschland
ggplot(data = deutsche_Abflüge, mapping = aes(x=reorder(source.City, source.City, length), fill= source.Name))+
  geom_bar(color = "Black")+
  coord_flip()+
  labs(title = "Anzahl an Routen pro Stadt",subtitle = "Deutschland" , x="")+
  theme(legend.position = "None")

#Anzahl an Routen pro Stadt USA
ggplot(data = USA_Abflüge[USA_Abflüge$Zielflughäfen > 150,], mapping = aes(x=reorder(source.City, source.City, length), fill= source.Name))+
  geom_bar(color = "Black")+
  coord_flip()+
  labs(title = "Anzahl an Routen pro Stadt",subtitle = "USA" , x="")+
  theme(legend.position = "None")

#Belibteste Zielländer Deutschland
ggplot(data = deutsche_Abflüge[deutsche_Abflüge$Zielverbingen>10,], mapping = aes(x=reorder(destination.Country, destination.Country, length)))+
  geom_bar()+
  coord_flip()+
  labs(title = "Verbindungen zu Zielländern", subtitle = "Startend in Deutschland",x="")

#Belibteste Zielflughäfen Deutschland
ggplot(data = deutsche_Abflüge[deutsche_Abflüge$Zielflughäfen>22,], mapping = aes(x=reorder(destination.Name, destination.Name, length), fill= destination.Country))+
  geom_bar(color = "Black")+
  coord_flip()+
  labs(title = "Belibteste Zielflughäfen",subtitle = "Startend in Deutschland", x="")


#Belibteste Zielländer USA
ggplot(data = USA_Abflüge[USA_Abflüge$Zielländer>30,], mapping = aes(x=reorder(destination.Country, destination.Country, length)))+
  geom_bar()+
  coord_flip()+
  labs(title = "Verbindungen zu Zielländern", subtitle = "Startend in der USA",x="")


ggplot(data = daten[daten$ausgehende_routen>350,], mapping = aes(x=reorder(source.Name, source.Name, length), fill = source.Country))+
  geom_bar()+
  coord_flip()+
  labs(title = "Größte Flughäfen", subtitle = "basierend auf Verbindungen",x="")

#Größte Fluglinien
ggplot(data = daten[daten$Anzahl_Verbindungen>650,], mapping = aes(x=reorder(airline.name, airline.name, length), fill = airline.country))+
  geom_bar()+
  coord_flip()+
  labs(title = "Größte Fluglinien", subtitle = "basierend auf Verbindungen",x="")
