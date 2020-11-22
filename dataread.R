library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)
library(rgdal)
library(dplyr)
library(plotly)
library(mapproj)
library(viridis)
library(broom)
library(tmap)
set.seed(420)
my_spdf <- readOGR(dsn="C:/Users/Danie/Desktop/MSOA2011", layer="Middle_Layer_Super_Output_Areas__December_2011__Boundaries")
spdf_fort <- fortify(my_spdf, region="msoa11cd")
setwd("C:/Users/Danie/Desktop/MSOA2011")
poly_lmsoa <- raster::shapefile("Middle_Layer_Super_Output_Areas__December_2011__Boundaries")


setwd("C:/Users/Danie/Desktop/populationestimates/")
popData <- read.csv(file = 'C:/Users/Danie/Desktop/populationestimates/popestimates.csv', header=TRUE)
salispop <- dplyr::filter(popData, ï..MSOACode %in% c("E02006666", "E02006667", "E02006668", "E02006670", "E02006671", "E02006673", "E02006674"))
colnames(salispop)[which(names(salispop) == "ï..MSOACode")] <- "id"
merged <-merge(spdf_fort,salispop, by="id")
final.data<-merged[order(merged$order), ]
final.data$All.Ages <- as.integer(gsub(",","",final.data$All.Ages))

censusdata <- read.csv(file = 'C:/Users/Danie/Desktop/populationestimates/2011censusSW.csv', header=TRUE)
saliscensus <- dplyr::filter(censusdata, MSOA.Code %in% c("E02006666", "E02006667", "E02006668", "E02006670", "E02006671", "E02006673", "E02006674"))
saliscensus$All.usual.residents <- as.integer(gsub(",","",saliscensus$All.usual.residents))
colnames(saliscensus)[which(names(saliscensus) == "MSOA.Code")] <- "id"
censusmerged <-merge(spdf_fort,saliscensus, by="id")
final.census.data<-censusmerged[order(censusmerged$order), ]
final.census.data$All.Ages <- as.integer(gsub(",","",final.data$All.Ages))

q <- ggplot(data=final.data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = All.Ages),
               colour = alpha("black", 1/2), size = 0.3)+
  theme_void()+
  labs(
    title = "2019",
    subtitle = "",
    caption = "World"
  ) +
  scale_fill_distiller(palette = "Greens" , na.value="white", direction=1, n=9, name="Population", guide = guide_colorbar( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm")) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig2 <- ggplotly(q)
fig2

p <- ggplot(data=final.census.data, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = All.usual.residents),
               colour = alpha("black", 1/2), size = 0.3)+
  theme_void()+
  labs(
    title = "2011",
    subtitle = "",
    caption = "World"
  ) +
  scale_fill_distiller(palette = "Greens" , na.value="white", direction=1, n=9, name="Population", guide = guide_colorbar( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm")) ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 16, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )  +
  coord_map() #Adds Mercator Projection

fig3 <- ggplotly(p)
fig3

