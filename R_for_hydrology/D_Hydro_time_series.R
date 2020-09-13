
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")


########
### Data
########

# Read D-Hydro output file
d = read.csv("data/Water depth at pressure points.csv", header=T, sep=",")
head(d)

# Extract the first column for dates and convert to time class
date = strptime(as.character(d[,1]), format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Also write date as numeric (format YYYYmmddHHMM)
date_num = as.numeric(paste0(substr(d[,1],1,4),substr(d[,1],6,7),substr(d[,1],9,10),
                             substr(d[,1],12,13),substr(d[,1],15,16)))

# Extract the second column for water depth
h = d[,2]


##############
### Processing
##############

### Generate labels for x-axis

# # Option 1: automatically:
# # Format x-axis: for 5 date labels on the x-axis, equally distributed.
# place_xlabel = date[1] + c(0:4) * (date[length(date)] - date[1])/4
# xlabel       = substr(as.character(place_xlabel),1,16)

# Option 2: manually:
# Specify at which time stamps you want a label (in YYYmmddHHMM).
# The result is the index belonging to that time.
xlabel_idx = c(which(date_num==199502140000), which(date_num==199502140600),
               which(date_num==199502141200), which(date_num==199502141800),
               which(date_num==199502150000))
# Specify which label you want at the tick marks.
# With empty space within "" you can move the label (the whole string is centered)
xlabel = c("     00:00", "06:00", "12:00", "18:00", "00:00     ")
# Convert label to the right format.
place_xlabel = as.POSIXct(date[xlabel_idx])


### Find maxima
# Maximum h
max_h = max(h)
# Date belonging to maximum
max_date = date[h == max_h]


##########
### Output
##########

# Open pdf
pdf("figs/D_Hydro_time_series.pdf", width=6, height=3, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl)
par(mar=c(1.1,2,0.5,0.5), mgp=c(1.2,0.2,0), tcl=-0.2) 

# Plot water level.
plot(date, h, type="l", col="dodgerblue", 
     ylim=c(0,2), 
     ylab="h [m]",
     xaxt="n", xaxs="i", yaxs="i")

# Add lines for maximum
lines(x=c(date[1], max_date), y=c(max_h,max_h), 
      col="grey", lty=2)
lines(x=c(max_date, max_date), y=c(0,max_h), 
      col="grey", lty=2)
text(x=max_date-1500, y=max_h+0.1, labels=substr(max_date, 11,16), col="grey", cex=0.8)


# Add x-axis
axis(side=1, at=place_xlabel, labels=xlabel)

# Close pdf
graphics.off()
