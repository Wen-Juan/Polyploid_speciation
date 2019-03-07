install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
library(ggplot2)
library(ggmap)
library(maps)

setwd("~/Polyploid_speciation/input/")

locations <- read.csv("pop_locations.csv", header = T)
names(locations)
print(locations)

latitude <- locations$Latitude 
longitude <- locations$Longitude
pop <- locations$Name_publication
survey.point <- locations$Morph

##########Important to run the below scripts successfully, you need to register a Google API key.
##########To obtain an API key and enable services, go to https://cloud.google.com/maps-platform/

basemap <- get_map(location="Iberian Peninsula", zoom = 6, scale = "auto", maptype="toner-lite", color=c("bw"),crop=TRUE)
ggmap(basemap)

map1 <- ggmap(basemap, base_layer=ggplot(locations, aes(x=Longtitude, y=Latitude)))
print(map1)

map.survey <- map1 + geom_point(aes(color = survey.point), size = 5, alpha = 1)
map.survey <- map.survey + labs(x="Longitude", y="Latitude", color="inflorescence structure")
map.survey <- map.survey + scale_colour_hue(name = "", breaks=c("P-", "P+"))
map.survey <- map.survey + theme(plot.title=element_text(face = c("bold"),colour = "black", size = 2)) + theme(axis.text.x = blue.bold.italic.16.text)
print(map.survey)

