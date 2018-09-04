

################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(RNetCDF)


########
### Data
########

# Open and read NetCDF file (it will become a list)
d = read.nc(open.nc("data/soilmoisture_test.nc", unpack=FALSE))


##############
### Processing
##############

# Find out which headers the list contains
names(d)

# Extract data
d$SM1


##########
### Output
##########



