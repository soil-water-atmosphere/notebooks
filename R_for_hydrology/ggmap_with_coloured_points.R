
# Script made by Alba Mols


################
### Initializing
################


# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# load packages
library(ggplot2)
library(ggmap)


########
### Data
########

# Read the KNMI data
KNMI=read.csv("data/KNMI_temp.txt", header=TRUE, sep="", )

# Extract Netherlands from maps dataset
NL=map_data("world","netherlands")


##############
### Processing
##############

# remove rows with missing data
KNMI=na.omit(KNMI, cols="T")


##########
### Output
##########

# Open pdf
pdf("figs/ggmap_with_coloured_points.pdf", family="Times", width=5, height=5)

# Set plotting parameters
par(mar=c(0,0,0,0))

# Make map of the Netherlands
NL_map = ggplot(NL, aes(x=long, y=lat, group=group)) + 
  geom_path(colour = 'black') +
  coord_map() + 
  theme_void() +
  labs(title = "Maximum temperature",                            
       subtitle = "Friday 01/01/2021")

# Add coloured points
NL_map + 
  geom_point(data=KNMI,aes(x=LON, y=LAT, colour=T), inherit.aes=FALSE, size=4, shape=16) +
  scale_color_gradient(low="yellow", high="red", "T [°C]")
  
# Close pdf
graphics.off()