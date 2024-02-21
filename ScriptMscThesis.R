#Script for my Msc thesis project: 

### SO here as I will add some code, everyone will be able to see it. 

###@Catherine, how does it work tho with you commenting my code etc...? 






#--------Master thesis project script----------

setwd("C:/Users/adria/OneDrive - Queensland University of Technology/Thesis/MscThesisProject")

##Libraries to load: 
library(gdata)
library(readxl)
library(sp)
library(raster)
library(rlang)
library(ggplot2)
library(leaflet)
library(rnaturalearth)
library(data.table)
library(vctrs)
library(terra)
library(robis)
library(tidyverse)
library(hexbin)
library(patchwork)
library(RSQLite)
library(magrittr)
library(ggrepel)
library(ggspatial)
library(mapsf)
library(reshape)
library(vegan)

#Importing world map and view of the study area: 
landMass=shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
plot(landMass, main="World Land Mass", col="lightGrey", axes=T, ylab="Latitude", xlab="Longitude")
StudExtent <- extent(149, 156, -34, -21)
plot(StudExtent, add=T, col="Red")

#define the study area in Australia: 
AUSBroadExtent <- extent(110, 160, -50, -5)
AUSBroadLandMass=crop(landMass, AUSBroadExtent)
plot(AUSBroadLandMass, main="World Land Mass", col="lightGrey", axes=T, ylab="Latitude", xlab="Longitude")
plot(StudExtent, add=T, col="Cyan", lwd=4)


#Crop of Study area: 
StudExtent <- extent(149, 156, -34, -21)
StudLandMass=crop(landMass, StudExtent)
plot(StudLandMass,main="Australian Western coast Crop", col="lightGrey", border="black", axes=T, ylab="Latitude", xlab="Longitude" )


sites <- read.csv("C:/Users/adria/OneDrive - Queensland University of Technology/Thesis/Data/AF_Australia_SitesMar2010toSep2023SUM.csv")
sites=na.omit(sites)
auCities <- read.csv("C:/Users/adria/OneDrive - Queensland University of Technology/Thesis/Data/au.csv", header=T)
auCities_clean = auCities%>%
  filter(population>100000)%>%
  filter(lng>149)%>%
  filter(lng<156)%>%
  filter(lat> -34)%>%
  filter(lat< -21)


MapSites= ggplot() + geom_polygon(data = StudLandMass, fill = "Grey", colour = "Black", linewidth = 0.3, aes(long, lat, group = group)) + ggtitle("Study Sites Map", ) + theme(plot.title = element_text(hjust = 0.5, size=20)) + xlab("Longitude") + ylab("Latitude") + coord_equal(ratio=1) + geom_point(data=sites, aes(x=Long, y=Lat), size=4, shape=23, fill="cyan") + geom_text_repel(data=auCities_clean, aes(x=lng, y=lat, label=city), nudge_x = c(-1.25), nudge_y = c(0.15)) + annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),style = north_arrow_fancy_orienteering)


MapSites

#if wanted to put blue as the ocean: 
#theme_test() + theme(panel.background = element_rect(fill = "lightblue")) + guides(size = "none")


#Insert on that map a broad vision of Australia on it: 








# Biomass data download and clean: 
Biomass2019=read_csv("Data/AUS_LongTransect_Subtropical_fish_biomass-2019_StartWorking.csv")
head(Biomass2019)
Biomass2019=Biomass2019[,1:12]
head(Biomass2019)
str(Biomass2019)
Biomass2019$a=as.numeric(Biomass2019$a)
Biomass2019$b=as.numeric(Biomass2019$b)
Biomass2019$`B (g)`=as.numeric(Biomass2019$`B (g)`)
# here should have the steps of Biomass standardisation: 
########


# NA removal 
Biomass2019=Biomass2019[complete.cases(Biomass2019), ]


# Site by species matrix: 
site.sp.mat=cast(Biomass2019, Year+id~Fish, value="B (g)", fun.aggregate=mean)
site.sp.mat=as.data.frame(site.sp.mat)
site.sp.mat[is.na(site.sp.mat)]=0
head(site.sp.mat)

# Bray-Curtis dissimilarity matrix calculation: 
bray.dist.site.sp.mat=vegdist(site.sp.mat, method="bray")


#Load the site decriptive csv
locs=read.csv("Data/AF_Australia_SitesMar2010toSep2023SUM.csv")
str(locs)

#bind it to the Biomass data set
Biomass2019.2=left_join(Biomass2019, locs, by=c("Site"="Site"))
str(Biomass2019.2)
summary(is.na(Biomass2019.2$Lat))
which(is.na(Biomass2019.2$Lat))





