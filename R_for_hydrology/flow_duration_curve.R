
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

# Read discharge data
d = read.table("data/Q_TestRiver_hour.txt", header=TRUE)


##############
### Processing
##############

# compute all percentiles
Q_percentiles = quantile(d$Q, probs=seq(0.99,0.01,-0.01), na.rm=TRUE)


##########
### Output
##########

### Plot original and moving average together

# Open pdf
pdf("figs/flow_duration_curve.pdf", width=4, height=3)

# Make margins smaller
par(mar=c(4,4,0.5,0.3))

# Plot exceedance probability on the x-axis and discharge on the y-axis. 
# Note: if you only specify one thing to plot, R will put that on the y-axis and the 
# index number (which is here 1 to 100, so exactly what you want) on the x-axis.
plot(Q_percentiles, 
     log="y", type="l",
     xlab="Exceedance [%]", ylab="Q [mm/d]")

# Close pdf
graphics.off()



### Write output to file
write.table(round(Q_percentiles, 3), "output/flow_duration_curve.dat", row.names=FALSE)


