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





#In welchen Länder gibt es die meisten Flughäfen?

Airports <- read.csv2(file=file.choose())

worldmap <- ne_countries(scale = "medium", returnclass = "sf")

#world<-left_join(worldmap, Airports)


ggplot(data = worldmap) +
  geom_sf()

#DataFrame zum ploten erstellen
Top3Countries <- data.frame(table(Airports$Country)) %>%
  arrange(desc(Freq)) %>%
  head(5)

ggplot(data = world) +
  geom_sf(data = Top3Countries, mapping = aes(fill=Var1))+
  labs(title = "Top 5 Countries with the highest number of airports")
    


ggplot(Airports, mapping = aes(x = Country))+
  data.frame(table(Airports$Country))+
  arrange(desc(Freq)) %>% 
  head(20) %>% 
  ggplot(aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1, label = Freq)) + 
  geom_bar(stat = "identity", show.legend = F) +
  labs(title = "Top 20 Countries that has most Airports", 
       x = "Country", y = "The number of Airports") +
  geom_label(angle = 45, show.legend = F) +
  theme(axis.text.x = element_text(angle = 40, size = 15))


#Wo sind rein visuell die meisten Flughafen?
worldmap <- ne_countries(scale = "medium", returnclass = "sf")

world<-left_join(worldmap,Airports)


ggplot(data = world) +
   geom_sf(data = Airports, x = Longitude, y = Latitude)




