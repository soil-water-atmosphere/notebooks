# Bootstrapping is a technique with which you can test 
# the effect of sampling uncertainty on your results.


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
d = read.table("data/calibrate_Q_h.txt", header=TRUE)


##############
### Processing
##############

# Determine how often you wat to resample. 
# This can be more than the sample size.
N = 100

# Make an empty matrix for the things you want to compute for each sample.
# So here you want the coefficients of the Q-h-relation
M = matrix(ncol=2, nrow=N)

# Run a loop N times (for each new sample).
for(i in 1:N)
{
  # Make a new sample of the measurement points with the same number of points as the original dataset.
  # First resample the element numbers.
  idx = sample(x=1:nrow(d), size=nrow(d), replace=TRUE)
  
  # Select these elements from the dataset.
  h = d[idx,1]
  Q = d[idx,2]
  
  # Fit a power law as Q-h-relation with nonlinear least squares.
  fit = nls(Q ~ a * h ^ b, start=list(a=1, b=1))
  
  # Retrieve the parameters from the fit.
  M[i,] = coef(fit)
    
}


##########
### Output
##########

### Plot the output of all fits together

# Open pdf
pdf("figs/bootstrap.pdf", width=4, height=4, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl)
# and make the figure square (pty).
par(mar=c(2.4,2.4,0.1,0.1), mgp=c(1.2,0.2,0), tcl=-0.2, pty="s") 

# Plot discharge
plot(d$h, d$Q, xlim=c(0,max(d$h)), ylim=c(0,max(d$Q)),
     type="n", # don't actually plot the points - you'll add them later on top 
     xlab="h [m]",
     ylab=expression(paste("Q [m"^{3}," s"^{-1},"]")))

# Draw the N curves for different samples in transparent colors 
# (the last two numbers of the color code is the opacity).
for(i in 1:N){
  curve(M[i,1]*x^M[i,2], add=TRUE, col="#1E90FF20")
}

# Add the points on top.
points(d$h, d$Q, pch=16)

# Close pdf
graphics.off()


