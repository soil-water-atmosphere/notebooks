
########################
### Initializing
########################


# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load package with example maps
library(maptools)
library(sp)
library(mapview)
library(leaflet)
library(rgdal)
library(maps)
library(mapview)

########
### Data
########

# Read gauge information
gauge_info = read.table("data/gauge_info.dat", header=TRUE)

# Make dummy values (for this example)
Qyear1 = runif(nrow(gauge_info), min=200, max=800)
Qyear2 = runif(nrow(gauge_info), min=200, max=800)
Qdata  = data.frame(Qyear1, Qyear2)

##############
### Processing
##############

coords = SpatialPoints(data.frame(gauge_info[,4:3]),
                       proj4string = CRS("+proj=longlat"))
Qdummy = SpatialPointsDataFrame(coords, data=Qdata)



##########
### Output
##########

# Define number of colors in figure
ncols = 7

# Make color range: change colors to preference (2 or more)
cols = colorRampPalette(c("lightyellow","blue"))(ncols)

# See what range of data is
range(Qdata, na.rm=TRUE)

# Define range 
col_min  = 200
col_max  = 800

# Make color scale
breaks=seq(col_min, col_max, length.out=ncols)

### Plot map
map = mapView(Qdummy, zcol=c("Qyear1","Qyear2"), color="transparent", col.regions=cols, 
              at=breaks, legend=TRUE, alpha=0)
map


# click "export"in plot window to get html (you can open that in a browser)
# click "export"in plot window to get png








