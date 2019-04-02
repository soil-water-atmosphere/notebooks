
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
d = read.table("data/Q_4TestRivers_day.dat", header=TRUE)

# Remove first column (dates)
Q = d[,2:ncol(d)]


##############
### Processing
##############

# Define what low and high flow thresholds are.
Q_low = 0.1
Q_high = 3

# Loop over rivers
n_low = c()
n_high = c()
for(i in 1:ncol(Q)){
  # compute fraction of low/high flow days
  frac_low  = length(Q[,i][Q[,i] < Q_low ]) / length(Q[,i])
  frac_high = length(Q[,i][Q[,i] > Q_high]) / length(Q[,i])
  # convert to average number of low/high flow days per year
  n_low [i] = round(frac_low  *365)
  n_high[i] = round(frac_high *365)
}

# # Note: if you have many rivers, don't use a for-loop, but use apply instead (it's faster):
# n_high = apply(Q, MARGIN=2, FUN=function(x){length(x[x>Q_high]) / length(x) *365})


##########
### Output
##########

# Define colors of the lines and IDs
cols = c("purple","dodgerblue","forestgreen","darkorange")
IDs  = c("River1","River2","River3","River4")

# Open pdf
pdf("figs/distributions.pdf", width=4, height=3, family="Times")

# Change margins (mar), distance of axes and labels (mgp) and tick marks (tcl)
par(mar=c(2.3,2.15,0.1,0.1), mgp=c(1.2,0.2,0), tcl=-0.2) 

# For full control of the layout, first make an empty plot with nice axes.
plot(1,1, type="n", xlim=c(0,5), ylim=c(0,1.7), xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
axis(side=1, at=seq(0,5,1), label=c("0","1","2","3","4","5 "))
axis(side=2, at=seq(0,1.5,0.5), label=seq(0,1.5,0.5))
mtext(side=1, line=1.4, expression(paste("Q [mm d"^{-1},"]")))     
mtext(side=2, line=1.3, "Density") 

# Then add the distributions as separate lines (run over each column)
for(i in 1:ncol(Q)){lines(density(Q[,i], na.rm=TRUE), col=cols[i])}

# Add a vertical lines to indicate low and high flows
abline(v=c(Q_low,Q_high), lty=3, col="grey")

# Add legend 1: IDs (and space for numbers)
legend(paste(c("",IDs),"                         "), x="topright", col=c("transparent",cols), bty="n", lty=1, cex=0.8)
# Add legend 2: number of high flow days
legend(paste(c("# low", n_low), "             "), x="topright", col="transparent", bty="n", lty=1, cex=0.8)
legend(c("# high", n_high), x="topright", col="transparent", bty="n", lty=1, cex=0.8)

# Close pdf
graphics.off()

