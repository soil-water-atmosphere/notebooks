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
d_all = read.table("data/d_locs_runs.txt", header=TRUE)

# Remove 1st column (run names)
d = d_all[,2:ncol(d_all)]

##############
### Processing
##############

# Define colors for locations
cols = c("red", "orange", "forestgreen", "dodgerblue")


##########
### Output
##########

### Plot original and moving average together

# Open pdf
pdf("figs/combined_barplot.pdf", width=6, height=4, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl)
par(mar=c(1.1,2.4,0.2,0.2), mgp=c(1.2,0.2,0), tcl=-0.2) 

# Make values for x-axes: start and end of line segments
x_values = c(0,2,3,5,6,8,9,11)
x_middle = c(1,4,7,10)

run_ID = c("run 1","run 2","run 3","run 4")
loc_ID = c("loc 1","loc 2","loc 3","loc 4")


# Make empty plot
plot(x_values, x_values, ylim=c(0,12), 
     type="n", xlab="", ylab="d [m]", xaxt="n")

# Run over each row (run)
for(nr_run in 1:nrow(d)){
  # Run over each column (location)
  for(nr_loc in 1:ncol(d)){
    
    #Plot each row (run) as a column and each item in the row (locaction) as line segment
    lines(x_values[(nr_run*2-1):(nr_run*2)], 
          c(d[nr_run,nr_loc],d[nr_run,nr_loc]), 
          col=cols[nr_loc], lwd=2)
    
  }
}

# Add x-axis
axis(side=1, at=x_middle, labels=run_ID, tcl=0)


# Add legend
legend(loc_ID, col=cols, x="topleft", lty=1, bty="n")



# Close pdf
graphics.off()
