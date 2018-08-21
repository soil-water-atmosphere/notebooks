
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(mapview)
library(rgdal)


########
### Data
########

# Read shape file
catchment = readOGR("data/catchment_boundary.shp")


##############
### Processing
##############

# convert from Rijksdriehoekscoordinates
proj4string(catchment) = CRS("+init=epsg:28992")

# Make a MapView item from shapefile
m = mapView(catchment)


##########
### Output
##########

# Plot in interactive map
m

