

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

# Open and read NetCDF file (it will become a list)
d = read.table("data/P_gauge_radar.dat", header=TRUE)


##############
### Processing
##############

# Fit linear model
fit = lm(d$radar ~ d$gauge)
fit
a   = fit$coef[1]
b   = fit$coef[2]
r2  = summary(fit)$r.squared

# Compute cumulative sums
cum_gauge = cumsum(d$gauge)
cum_radar = cumsum(d$radar)

# Convert date-time to POSIX
date = strptime(d$date, format="%Y%m%d%H", tz="UTC")

# Format x-axis: for 4 date labels on the x-axis, equally distributed
place_xlabel = date[1] +c(0:3) *(date[length(date)] - date[1])/3
xlabel       = substr(as.character(place_xlabel),1,10)

# Compute cumulative distribution function
cdf_gauge = quantile(d$gauge, probs=seq(0,1,0.001))
cdf_radar = quantile(d$radar, probs=seq(0,1,0.001))


##########
### Output
##########


# Open pdf
pdf("figs/compare_time_series.pdf", width=6, height=6, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl).
# Make two rows and two columns and square figures.
par(mar=c(2.4,2.4,0.5,0.5), mgp=c(1.2,0.2,0), tcl=-0.2, mfrow=c(2,2), pty="s") 


### Fig 1: scatter plot

plot(d$gauge, d$radar, 
     xlim=c(0,max(c(d$gauge,d$radar))),                                      # make axes the same
     ylim=c(0,max(c(d$gauge,d$radar))),   
     pch=16, cex=0.4,                                                        # make small, solid points
     xaxs="i", yaxs="i",                                                     # remove white space at start and end
     xlab=expression(paste("P gauge [mm h"^{-1}, "]")), 
     ylab=expression(paste("P radar [mm h"^{-1}, "]"))) 

# add 1:1-line
curve(x*1, add=TRUE, col="purple")                                           
text("1:1", x=8, y=8.5, col="purple")                                        

# add regression line
curve(a+b*x, add=TRUE, col="dodgerblue")                                     
text(paste0("y=",round(a,2),"+",round(b,2),"x"), x=7.3, y=5, col="dodgerblue")
text(bquote(paste("r"^{2},"=",.(round(r2,2)))), x=8, y=6, col="dodgerblue")


### Fig 2: double mass curve (useful for identifying systematic deviations)

plot(cum_gauge, cum_radar, type="l",
     xlim=c(0,max(c(sum(d$gauge),sum(d$radar)))),                            # make axes the same
     ylim=c(0,max(c(sum(d$gauge),sum(d$radar)))),
     xaxs="i", yaxs="i",                                                     # remove white space at start and end
     xlab="Cum. sum P gauge [mm]", ylab="Cum. sum P radar [mm]")                                          

# add 1:1-line
curve(x*1, add=TRUE, col="purple")  
text("1:1", x=700, y=650, col="purple")   


### Fig 3: cumulative time series

plot(date, cum_gauge, type="l", col="forestgreen",
     ylim=c(0,max(c(sum(d$gauge),sum(d$radar)))),
     xaxs="i", yaxs="i", xaxt="n",                                           # remove white space at start and end
     xlab="", ylab="Cum. sum P [mm]")
lines(date, cum_radar, col="red")

# Add sums
text(bquote(paste(Sigma," P gauge = ",.(round(sum(d$gauge)))," mm")),
     x=date[200], y=700, adj=c(0,0.5), col="forestgreen")
text(bquote(paste(Sigma," P radar = ",.(round(sum(d$radar)))," mm")),
     x=date[200], y=650, adj=c(0,0.5), col="red")

# Add x-axis
axis(side=1, at=place_xlabel, labels=xlabel)                             


### Fig 4: cumulative distribution function

plot(cdf_gauge, seq(0,100,0.1), type="l", col="forestgreen", log="x",
     xlim=c(0.01,max(c(d$gauge,d$radar))), ylim=c(85,100),
     xaxs="i", yaxs="i",                                                     # remove white space at start and end
     xlab=expression(paste("P [mm h"^{-1}, "]")), 
     ylab="Exceedance probability [%]")
lines(cdf_radar, seq(0,100,0.1), col="red")
legend(c("gauge","radar"), col=c("forestgreen","red"), lty=1, x="topleft", bty="n")


# Close pdf
graphics.off()


