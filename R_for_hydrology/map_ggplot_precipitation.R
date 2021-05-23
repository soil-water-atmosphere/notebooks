
# Script made by Alba Mols


################
### Initializing
################


# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(ggplot2)
library(ggvoronoi)
library(rgdal)


########
### Data
########


# Read shape file of border and provinces of the Netherlands
NL_borders = readOGR("data/gadm36_NLD_1.SHP")

# Read KNMI precipitation data
KNMI=read.csv("data/KNMI_20212701.txt", header=TRUE, sep="")


##############
### Processing
##############

# Remove NA's from KNMI data
KNMI=na.omit(KNMI, cols="P")


##########
### Output
##########


# Open pdf
pdf("figs/map_ggplot_precipitation.pdf", family="Times", width=5, height=5)

# Plot precipitation map
NL_map =
  ggplot(data=KNMI,aes(x=LON,y=LAT)) +
  scale_fill_gradientn("mm",                                            # define colour scale
    colors=(c("slategray1", "dodgerblue", "royalblue3"))) +
  coord_quickmap() +                                                    # set coordinate system map
  theme_void() +                                                        # set layout map
  labs(title = "Precipitation: 24-hour sum",                            # add title and subtitle
    subtitle = "Wednesday 27/01/2021")

NL_map +
  stat_voronoi(aes(fill=P),outline=NL_borders)+                          # make coloured regions, and use the shapefile of the Netherlands as a mask 
  geom_path(data=NL_borders,aes(long,lat,group=group),color="white")+    # add borders of Dutch provinces
  geom_text(label=KNMI$P, size=3)                                       # add precipitation sums as labels on the map


# Close pdf
graphics.off()

