
# Script made by Alba Mols


################
### Initializing
################


# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(rJava)
library(OpenStreetMap)


########
### Data
########


# Read the KNMI data
KNMI=read.csv("data/KNMI_temp.txt", header=TRUE, sep="", )


##############
### Processing
##############


# Make an OpenStreetMap
map=openmap(c(53.737635, 2.970863),            # Upper left coordinates
            c(50.595688, 7.632174))            # Lower right coordinates

# Convert station coordinates from longitude and latitude to Mercator projection
mercator=projectMercator(KNMI$LAT, KNMI$LON)
KNMI$LON=mercator[,1]
KNMI$LAT=mercator[,2]

# Remove rows with missing data
KNMI=na.omit(KNMI, cols="T")


### Make colour scale map

# Make color range (the function "colorRampPallete" could also be used here)
cols = c("yellow", "gold", "orange","red", "firebrick3","darkred")

# Define temperature range
col_min  = 3
col_max  = 9

# Make a vector of the colour each temperature value belongs to
colours = cols[KNMI$T-(col_min-0.9)]


##########
### Output
##########


# Open pdf
pdf("figs/openstreetmap_coloured_points.pdf", family="Times", width=6, height=5)

# Set plotting parameters
par(mar=c(1,1,1,1))


### Plot map

# Plot map of the Netherlands
plot(map, removeMargin=FALSE, asp=4)
text(x=558000, y=7100000, "Maximum temperatures on friday 01-01-2021", cex=1.2)

# Add coloured points to map
points(KNMI$LON, KNMI$LAT, pch=16, col=colours, cex=1.5)


# Add legend
legend(x=319000, y=7080000, 
       bty="n",                                                    
       pch=16,
       cex=1,
       col=cols, 
       legend=c("3.1 - 4","4.1 - 5","5.1 - 6","6.1 - 7","7.1 - 8", "8.1 - 9"),
       title="T in °C",
       y.intersp=0.9,                                        # change space between points and text
       x.intersp=0.7
       )

# Close pdf
graphics.off()
