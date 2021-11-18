install.packages('rgdal')
install.packages('sp')
install.packages('sf')
install.packages('ggplot2')
library(rgdal)
library(sp)
library(sf)
library(ggplot2)

#set working directory
setwd("C:\\Users\\LENOVO\\Desktop\\GIS_532_Data_Analysis\\Labs\\Data\\W4_RCode_Data")

#read shape files
stores <- readOGR('Stores.shp', layer='Stores')
zones <- readOGR('Zones.shp', layer='Zones')

# To extract coordinate system (CS) information from an sf object, use the st_crsfunction.
st_crs(stores)
summary(stores)
summary(zones)

# View feature counts i.e. rows/ # of points in shapefile
length(stores)

# View first 6 attribute names and values in data
head(stores)

# View attribute names
names(stores)

# View all values in attribute “Name” in stores shapefile
stores$Name
stores$Type
# View unique values in attribute “Name” in stores shapefile
# Notice the difference in “Levels” for each line of code below:
factor(stores@data$Name) # Type string
factor(stores@data$Type) # Type string
factor(stores@data$Zip) # Type double

# Select a subset of features form shapefile i.e. select only those points who have name -> Pepe
stores[stores$Type == 'Pepe',]
stores[stores$Type == 'Buena Vida',]

# Save the subset of data created above:
stores_buena <- stores[stores$Type == 'Buena Vida',]
length(stores_buena)

stores_pepe <- stores[stores$Type == "Pepe’s",]
length(stores_pepe)

stores_zip <- stores[stores$Zip == '76119',]
length(stores_zip)

# Plot the subset data
# 1
plot(stores, main="PePe's #322 Store Locations", pch=2,
     cex=0.5, col='red')
# 2
plot(stores_pepe, main="PePe's #322 Store Locations", pch=2,
     cex=0.5, col='green')
# 3
plot(stores_buena, main = "PePe's #322 Store Locations", pch = 2, cex = 0.5, col ="blue")
# 4
plot(stores_zip, main = "PePe's #322 Store Locations", pch = 2, cex = 0.5, col ="orange")

# Convert shapefile to to sf object
map_stores <- st_as_sf(stores)
map_zones <- st_as_sf(zones)

# Plot all features in “map_stores”
ggplot() + geom_sf(data = map_stores) + ggtitle("Store Locations", subtitle ="Names") + coord_sf()
ggplot() + geom_sf(data = map_zones) + ggtitle("Zones", subtitle = "") + coord_sf()

# Plot features based on attributes in “stores”. We use attribute “zip”to plot points that belong to that zip code:
ggplot() + geom_sf(data = map_stores,
                   aes(color = Type)) + labs(color = 'Type') + ggtitle("Store Locations"
                                                                       , subtitle = "Type") + coord_sf()
ggplot() + geom_sf(data = map_stores,
                   aes(color = Zip)) + labs(color = 'Zip') +ggtitle("Store Locations"
                                                                    , subtitle = "Zip Code") + coord_sf()

# 1: Plot polygon shapefile in colors:
ggplot(data=map_zones) + geom_sf(fill='thistle1', color='black')
# 2: Plot polygon shapefile in colors :
ggplot() + geom_sf(data=map_zones, fill='springgreen1', color='black')

# 1: Plot polygon shapefile based on attibute type “zones”:
ggplot()+geom_sf(data=map_zones, aes(fill='zones'))
# 2: Plot polygon shapefile based on attibute type “zones”:
ggplot() + geom_sf(data = map_zones, aes(color = Zones)) + labs(color = 'Zones')+
  ggtitle("Store Locations", subtitle = "Zip Code") + coord_sf()


# 1: Plot stores points shapefile on top of zones polygon shapefile
ggplot() + geom_sf(data = map_zones) + geom_sf(data = map_stores, aes(color= Zip)) + labs(color = 'Zip')+ ggtitle('Store Locations', subtitle = "Zip Code") +coord_sf()
# 2: Plot stores points shapefile on top of zones polygon shapefile
ggplot() + geom_sf(data = map_zones, fill = "darkseagreen1", color = "black") + geom_sf(data= map_stores, aes(color = Zip)) + labs(color = 'Zip') + ggtitle("Store Locations",subtitle = 'Zip Code') + coord_sf()

# Create custom color scheme for maps:
my_colors_1 <- c('tomato','royalblue2','orange2','turquoise1','forestgreen')
my_colors_2 <- c('tomato', 'blue')

# Apply custom color scheme to display polygons in a specific color scheme createdabove:
# 1: Apply custom color scheme to display of points calls layer:
ggplot() + geom_sf(data = map_stores, aes(color = Type)) +scale_color_manual(values = my_colors_2) + labs(color = 'Type') + ggtitle('StoreLocation', subtitle = "Type") + coord_sf()
# 2: Apply custom color scheme to display of both layers together:
ggplot() + geom_sf(data = map_zones) + geom_sf(data = map_stores, aes(color =Type)) + scale_color_manual(values = my_colors_2) + labs(color = 'Type') +ggtitle('Store Location', subtitle = 'Type') + coord_sf()
# 3: Apply custom LEGEND to display of both layers together:
ggplot() + geom_sf(data = map_zones) + geom_sf(data = map_stores, aes(color =Type)) + scale_color_manual(values = my_colors_2) + labs(color = 'Type') +theme(legend.text = element_text(size = 10), legend.box.background =element_rect(size = 2)) + ggtitle('Store Location', subtitle = 'Type') + coord_sf()

# Plot 2 shapefiles
plot(zones, col='grey')
plot(territories, col='grey', add=TRUE)
plot (stores, pch = 0.5, cex = 0.1, col = 'red', add = TRUE) # change size and shape ofpoints
title ("Calls on Feb 15")

# Analyze Data Distributions – SPATIAL
stores_coords <- as.data.frame(coordinates(stores))
View(stores_coords)
plot(stores_coords,pch=19)

# Spatial MEAN: for stores points:
stores_X_MEAN <- mean(stores_coords$coords.x1); stores_X_MEAN
stores_Y_MEAN <- mean(stores_coords$coords.x2); stores_Y_MEAN
# Spatial MEDIAN: : for stores points:
stores_X_MED <- median(stores_coords$coords.x1); stores_X_MED
stores_Y_MED <- median(stores_coords$coords.x2); stores_Y_MED
# Standard Deviational Ellipse (SDE)
install.packages('aspace')
library(aspace)

# Use function calc_sde to calculate SDE
calc_sde(id=1,points= stores_coords)
plot_sde(plotnew = FALSE, plotcentre = FALSE, centre.col='red2', centre.pch='1',sde.col='red2',sde.lwd=1,titletxt="", plotpoints = TRUE,points.col='grey4')
plot_sde(plotnew = TRUE, plotcentre = TRUE, centre.col='red', centre.pch="1",sde.col='red',sde.lwd=1,titletxt="", plotpoints = TRUE,points.col='black')

# Convert SDE to shapefile
shp <- convert.to.shapefile(sdeloc,sdeatt,'id',5)
shp
# Export SDE as shapefile
write.shapefile(shp, "SDE_Shape", arcgis=T)
SDE_shp <- readOGR('SDE_Shape.shp', layer='SDE_Shape')
SDE_shp
map_SDE_shp <- st_as_sf(SDE_shp)

# Plot stores point spatial mean shapefile (created above) on top of zones polygonshapefile (existing)
ggplot() + geom_sf(data = map_SDE_shp) + ggtitle("Spatial Mean", subtitle ='Stores') + coord_sf()


# Combine spatial mean X,Y coordinates using “cbind” i.e. combine columns:
stores_mean_coords <- cbind(stores_X_MEAN, stores_Y_MEAN)
stores_mean_coords
# Combine spatial median X,Y coordinates using “cbind” i.e. combine columns:
stores_median_coords <- cbind(stores_X_MED, stores_Y_MED)
stores_median_coords




