
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load package
library(lfstat)

########
### Data
########

# Read discharge data
d = read.table("data/Q_TestRiver_day.txt", header=TRUE)


##############
### Processing
##############

# Convert from number-format to date-format
date = as.Date(as.character(d$date), format="%Y%m%d")

# Convert to a Time-Series-class (specific for the lfstat-package)
Q_ts = ts(d$Q, frequency=(365)) 

# Convert to a low-flow-object class (specific for the lfstat-package)
Q_lfobj = createlfobj(Q_ts, startdate = "19760101", dateformat = "%Y%m%d")

# Extract baseflow
BF = Q_lfobj$baseflow

# Compute baseflow index
BFI = mean(BF/d$Q, na.rm=TRUE)


##########
### Output
##########

### Plot baseflow

# Select one year to plot (example):
# Find out which elements belong to that year
idx = which(d$date>20090000 & d$date<20100000)
# Extract the elements from the complete matrix/vectors
d_plot    = d [idx,]
date_plot = date [idx]
BF_plot   = BF[idx]

# Open pdf
pdf("figs/baseflow_index.pdf", width=6, height=3)

# Make margins smaller
par(mfrow=c(1,1), pty="m", mar=c(1.2,2.4,0.1,0.2), mgp=c(1.2,0.2,0), tcl=-0.2) 

# Plot discharge
plot(date_plot, y=d_plot$Q, type="n", xaxs="i", yaxs="i",
     xlab="", ylab="")
# Plot baseflow in orange. 
# For the polygon, you first go from left to right (increasing dates and values for baseflow)
# and then from right to left (decreasing dates and zero discharge).
polygon(x=c(date_plot, date_plot[366-1:365]), 
        y=c(BF_plot, rep(0,365)), col="orange")
# Plot quickflow in blue.
polygon(x=c(date_plot, date_plot[366-1:365]), 
        y=c(d_plot$Q, BF_plot[366-1:365]), col="dodgerblue")

# Add y-axis
mtext(side=2, line=1.2, substitute(paste("Discharge [m"^{3}," s"^{-1},"]")))

# Add value for BFI in the corner
text(bquote(paste("BFI = ",.(round(BFI,2)))), x=date_plot[35], y=0.35)

# Redraw box around plot (the polygon partly erased it)
box()

# Close pdf
graphics.off()



### Write output to file.

# Combine data into one table.
M = data.frame(d,BF=round(BF,3))

# Write to file.
write.table(M, "output/baseflow_index.dat", row.names=FALSE)






