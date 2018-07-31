
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
pdf("figs/moving_average.pdf", width=4, height=3)

# Plot discharge
plot(d_feb2011$Q, type="l", col="dodgerblue")

# Add moving average
lines(Qave, col="darkblue")

# Add legend
legend(c("original","48-h moving average"), 
       col=c("dodgerblue","darkblue"), 
       bty="n", lty=1, x="topleft")

# Close pdf
graphics.off()





