library(tidyverse)
library(ggplot2)

#variable reviewrbind is in final project code
cities <- as_tibble(reviewrbind$Location)
cities <- cities %>%
  group_by(value) %>%
  mutate(num = n())
cities <- unique(cities)


citycol <- c()
statecol <- c()
for (i in 1:nrow(cities)) {
  citycol <- c(citycol, strsplit(cities$value[i],", ")[[1]][1])
  statecol <- c(statecol, strsplit(cities$value[i],", ")[[1]][2])
}

cities <- cbind(cities, citycol, statecol)  
names(cities)[names(cities) == "...3"] <- "city"
names(cities)[names(cities) == "...4"] <- "state"

#uscities.csv is in the mgta452_project folder
uscities <- read_csv("uscities.csv")
uscities <- uscities %>%
  mutate(value = paste(uscities$city, uscities$state_id, sep=", "))

a <- inner_join(cities, uscities, by = "value")
a <- subset(a, select = c(value, num, lat, lng))



install.packages("leaflet")
install.packages("rgdal")
library(leaflet)
library(rgdal)


a$name <- paste0('<b>',a$value,'</b>','<br/>',a$num,' Job(s) Matching Your Resume')
names(a)[names(a) == "value"] <- "Location"


library(maps)
westcoast = map('state', regions = c('california', 'alaska', 'hawaii', 'oregon', 'washington'), fill = TRUE, plot = FALSE)
northeast = map('state', regions = c("maine", "vermont", "new hampshire", "massachusetts", "connecticut", "rhode island",
                                     "new york", "pennsylvania", "delaware", "district of columbia", "maryland"), fill = TRUE, plot = FALSE)
southeast = map('state', regions = c("kentucky", "new jersey", "tennessee", "virginia", "west virginia", "north carolina", "south carolina", "georgia", "florida"),
                fill = TRUE, plot = FALSE)
midwest = map('state', regions = c("wyoming", "south dakota", "north dakota","nebraska", "kansas", "oklahoma", 
                                   "missouri",  "texas", "arkansas", "louisiana"), fill = TRUE, plot = FALSE)
central = map('state', regions = c( "wisconsin", "illinois", "indiana", "michigan", "ohio",
                                    "minnesota", "iowa","alabama", "mississippi"), fill = TRUE, plot = FALSE)
mountain = map('state', regions = c("nevada", "arizona", "new mexico", "montana", "idaho",  "utah", "colorado"), fill = TRUE, plot = FALSE)

#variable regions is in final project code
regions <- na.omit(regions)

leaflet(a) %>%
  addTiles() %>%
  addPolygons(data = westcoast, color = 'blue', stroke = FALSE, popup=paste0(regions$n[regions$Region=='West'],' Jobs In This Region: ', '<b>West</b>'),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addPolygons(data = northeast, color = 'red', stroke = FALSE, popup=paste0(regions$n[regions$Region=='North East'],' Jobs In This Region: ', '<b>North East</b>'),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addPolygons(data = southeast, color = 'gold', stroke = FALSE, popup=paste0(regions$n[regions$Region=='South East'],' Jobs In This Region: ', '<b>South East</b>'),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addPolygons(data = midwest, color = 'purple', stroke = FALSE, popup=paste0(regions$n[regions$Region=='Midwest'],' Jobs In This Region: ', '<b>Midwest</b>'),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addPolygons(data = central, color = 'pink', stroke = FALSE, popup=paste0(regions$n[regions$Region=='Central'],' Jobs In This Region: ', '<b>Central</b>'),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addPolygons(data = mountain, color = 'lime', stroke = FALSE, popup=paste0(regions$n[regions$Region=='Mountain'],' Jobs In This Region: ', '<b>Mountain</b>'),
              popupOptions = popupOptions(closeOnClick = TRUE)) %>%
  addCircles(lng = ~lng, lat = ~lat, radius = ~(num)^2/3, popup = ~name, color = "#05B") %>%
  addProviderTiles("OpenStreetMap.Mapnik")
