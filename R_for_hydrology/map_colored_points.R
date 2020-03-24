
########################
### Initializing
########################


# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load package with example maps
library(maps)


########
### Data
########

# Read gauge information
gauge_info = read.table("data/gauge_info.dat", header=TRUE)

# Make dummy values (for this example)
Qyear = runif(nrow(gauge_info), min=200, max=800)


##############
### Processing
##############

# Extract Texas from maps dataset
states = map_data("state")
texas  = states[states$region=="texas",]


##########
### Output
##########

# Define number of colors in figure
ncols = 100

# Define number of colors in legend
ncols_legend = 7

# Make color range: change colors to preference
cols = colorRampPalette(c("lightyellow","blue"))(ncols)

# Say which variable you want to plot
variable = Qyear

# Define range 
col_min  = 200
col_max  = 800

# Check if all data are within range
min(variable, na.rm=TRUE) > col_min
min(variable, na.rm=TRUE) < col_max

# Look up which color belongs to each value in the variable's vector.
# (You don't have to change anything here for other plots - just run these lines).
colors = cols[ceiling((col_max-variable)/(col_max-col_min)*ncols)]

# Make colors and belonging numbers. 
# (You don't have to change anything here for other plots - just run these lines).
colors_legend  = cols[round(seq(1,ncols,length.out=ncols_legend))]
numbers_legend = round(col_min+seq(0,ncols,length.out=ncols_legend)/100*(col_max-col_min))


### Plot map

# Open pdf
pdf("figs/map_colored_points.pdf", width=4.2, height=4) # make sure the aspect (width/height matches reality)

# Remove margins
par(mar=c(0,0,0,0))

# plot map
plot(texas$long, texas$lat, type="l", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
points(gauge_info$LONG, gauge_info$LAT, pch=16, col=colors)

# legend
legend(x="topleft", bty="n", pch=16, col=colors_legend, legend=numbers_legend)

# Close pdf
graphics.off()




