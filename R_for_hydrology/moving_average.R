
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(zoo)


########
### Data
########

# Read discharge data
d = read.table("data/Q_TestRiver_hour.txt", header=TRUE)

# Select part of the whole time series 
d_feb2011 = d[d$date>2011020000 & d$date<2011030000,]


##############
### Processing
##############

# Interpolate missing values
Q = na.approx(d_feb2011$Q)

#Compute 24-hour moving average
Qave = rollmean(Q, k=24, fill="extend")


##########
### Output
##########

### Plot original and moving average together

# Open pdf
pdf("figs/moving_average.pdf", width=4, height=3, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl).
par(mar=c(2.4,2.4,0.1,0.1), mgp=c(1.2,0.2,0), tcl=-0.2) 

# Plot discharge
plot(d_feb2011$Q, type="l", col="dodgerblue", ylim=c(0,0.65),
     xlab="time [h]", ylab=expression(paste("Q [mm h"^{-1},"]")),
     xaxs="i", yaxs="i") # to remove space around the lines

# Add moving average
lines(Qave, col="darkblue")

# Add legend
legend(c("original","48h mov. ave."), 
       col=c("dodgerblue","darkblue"), 
       bty="n", lty=1, x="bottomleft")

# Close pdf
graphics.off()





