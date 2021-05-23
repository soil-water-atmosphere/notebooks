
# Script made by Alba Mols


################
### Initializing
################


# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(mapview)
library(sp)


########
### Data
########


# Read the KNMI data
KNMI=read.csv("data/KNMI_temp.txt", header=TRUE, sep="", )


##############
### Processing
##############


# Make data frame from station coordinates
stations = SpatialPoints(data.frame(KNMI[,2:3]),
                         proj4string = CRS("+proj=longlat"))

##########
### Output
##########


# Plot station locations on a satellite map
mapView(stations, 
        label=KNMI$NAME,                                  # get station names when hovering over the points
        map.types="Esri.WorldImagery",                    # set satellite as background
        color="white", 
        col.regions="white", 
        cex=5.5)                                          # make points smaller


# click "export" in plot window to get html (you can open that in a browser)
# click "export" in plot window to get png

