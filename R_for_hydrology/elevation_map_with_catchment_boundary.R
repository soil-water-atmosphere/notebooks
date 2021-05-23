
# Script made by Alba Mols


################
### Initializing
################


# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Clear memory
rm(list=ls())

# Load packages
library(raster)
library(rgdal)
library(sp)


########
### Data
########


# Get elevation map of the Netherlands
NL=getData('alt', country='NLD', mask=TRUE)

# Load chatchment boundary of the Aa catchment
catchment=readOGR("data/Aa.shp")


##############
### Processing
##############


# Make colour scale
cols=hcl.colors(n=25,palette="Terrain2")

# Transfer catchment to longitude and latitude projection
catchment=spTransform(catchment, CRS("+proj=longlat +datum=WGS84"))


##########
### Output
##########

# Open pdf
pdf("figs/elevation_map_with_catchment_boundary.pdf", family="Times", width=7, height=5)

# Set plotting parameters
par(mar=c(3,4,2,3))

# Make elevation plot
plot(NL, col=cols, 
     main="Elevation above see level (m)", 
     xaxt='n', yaxt='n'
     )

# Add catchment boundary
lines(catchment, col="black")

# Add legend
rect(2.67,53.44,2.96,53.54, col="forestgreen", border=FALSE)
legend(2.53,53.6,legend="Catchment boundary Aa", col="black", lty=1, cex=0.8, bty="n")


# Close pdf
graphics.off()
