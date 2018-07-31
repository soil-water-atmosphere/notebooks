
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load package for moving average
library(zoo)

########
### Data
########

# Read discharge data
d_all = read.table("data/PEQ_TestRiver_hour.dat", header=TRUE)

# Select part of the whole time series 
d = d_all[d_all$date>2017000000 & d_all$date<2018000000,]


##############
### Processing
##############

# Convert dates to date-format 
date = strptime(as.character(d$date), format="%Y%m%d%H", tz="UTC")

# Compute 5-day moving average of ETpot
ETpot_movave = rollmean(d$ETpot, k=5*24, fill=NA)

# format x-axis: for 7 date labels on the x-axis, equally distributed
place_xlabel = c(date[1] +c(0:6) *(date[length(date)] - date[1])/6)
xlabel       = substr(as.character(place_xlabel),1,10)


##########
### Output
##########

### Plot original and moving average together

# Open pdf
pdf("figs/plot_PEQ.pdf", width=6, height=4, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl)
par(mar=c(1.1,2.4,1,2.15), mgp=c(1.2,0.2,0), tcl=-0.2) 

# Plot evaporation.
plot(date, ETpot_movave, type="l", col="orange", 
     ylim=c(0,1.2*max(c(d$Q, d$ETpot), na.rm=TRUE)), 
     ylab=expression(paste("ET"[pot]," (5-day moving average) and Q [mm h"^{-1},"]")),
     xaxt="n", xaxs="i", yaxs="i")

# Add discharge.
lines(date, d$Q, col="dodgerblue")

# Add x-axis
axis(side=1, at=place_xlabel, labels=xlabel)

# Add precipitation with separate y-axis.
par(new=TRUE)
plot(date, d$P, type="h", col="purple", 
     ylim=c(max(d$P, na.rm=T)*5,0),
     xlab="", ylab="", xaxt="n", yaxt="n", xaxs="i", yaxs="i")

# Add right y-axis for P.
mtext(expression(paste("P [mm h"^{-1},"]")), side=4, line=1.2)
axis(side=4)

# Add legend
legend(c("P",expression(paste("ET"[pot])), "Q"),
       col=c("purple","orange","dodgerblue"), 
       x="left", lty=1, bty="n")



# Close pdf
graphics.off()





