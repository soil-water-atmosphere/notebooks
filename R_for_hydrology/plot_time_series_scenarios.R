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

# Read data
d = read.table("data/dG_scenarios.dat", header = TRUE)


##############
### Processing
##############

# Convert dates to date-format 
date = strptime(as.character(d$date), format="%Y%m%d", tz="UTC")

# format x-axis: location of tick marks and labels at each tick mark
place_xlabel = strptime(c("20110101","20110401","20110701","20111001"), format="%Y%m%d", tz="UTC")
xlabel       = c("1 Jan","1 Apr","1 Jul","1 Oct")

# set colors
cols = c("dodgerblue","forestgreen","orange")


##########
### Output
##########


# Open pdf
pdf("figs/time_series_scenarios.pdf", width=6, height=4, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl)
par(mar=c(1.1,2.2,0.6,0.1), mgp=c(1.2,0.2,0), tcl=-0.2) 

# Make an empty plot
plot(date, d[,1], type="n", col=cols[1], 
     ylim=c(1.5,0.6), 
     ylab=expression(paste("d"[G]," [m-sfc]")),
     xaxt="n", xaxs="i", yaxs="i")

# Add other series. Loop over all columns.
for(i in 1:(ncol(d)-1)){
  lines(date, d[,(i+1)]/1000, col=cols[i])
}

# Add x-axis
axis(side=1, at=as.numeric(place_xlabel), labels=xlabel)

# Add legend
legend(paste("scenario", 1:3),
       col=cols, 
       x="bottomleft", lty=1, bty="n")

# Close pdf
graphics.off()

