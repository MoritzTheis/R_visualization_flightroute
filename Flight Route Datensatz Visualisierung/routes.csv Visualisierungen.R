
library(ggplot2)
library(ggmap)
library(maptools)

routes <- read.csv(file=file.choose())

dim(routes)
head(routes)
tail(routes)

top20airlines <- data.frame(routes, as.factor(airline) >1000)

#Welche Airline besitzt die meisten Routen?
ggplot(data = routes, mapping = aes(x = airline))+
  geom_bar()+
  theme_classic()

#facet um zu zeigen welcher der Airlines die meisten Bereiche der Erde abdecken