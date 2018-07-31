
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/repository/")


########
### Data
########

# Read discharge data
d = read.table("data/Q_TestRiver_day.txt", header=TRUE)


##############
### Processing
##############

# Extract months:
# Divide date-column by 100 (to cut off the day) and round; 
# Divide date-column by 10000 (to cut off the month and day), round and add two zeroes;
# Subtract to get the months.
month = d$date%/%100 - d$date%/%10000*100

# Aggregate Q by month to get 3 vectors with elements for each month
# with means, one with 10% and one with 90% quantiles from January to December.
Q_mean = aggregate(x=d$Q, by=list(month), FUN=mean, na.rm=TRUE)            [,2]
Q_10   = aggregate(x=d$Q, by=list(month), quantile, probs=0.1, na.rm=TRUE) [,2]
Q_90   = aggregate(x=d$Q, by=list(month), quantile, probs=0.9, na.rm=TRUE) [,2]


##########
### Output
##########

### Plot monthly averages

# Open pdf
pdf("figs/monthly_averages.pdf", width=6, height=3)

# Make margins smaller
par(mar=c(4,4,0.5,1))

# Plot discharge
plot(c(Q_mean,Q_mean[12]),     # repeat the last value to get a horizontal line for December
     type="s", xlim=c(1,13), ylim=c(min(Q_10), max(Q_90)),
     xlab="Month", ylab="Q [mm/d]", 
     xaxt="n", xaxs="i")    # don't plot x-axis labels and remove extra space on x-axis

# Add 10th and 90th percentile
lines(c(Q_10,Q_10[12]), type="s", col="grey")
lines(c(Q_90,Q_90[12]), type="s", col="grey")

# Add x-axis: first tick marks and then months in between
axis(side=1, at=1:12, labels=rep("",12))
axis(side=1, at=1.5:12.5, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), tcl=0)

# Close pdf
graphics.off()



### Write output to file.

# Combine data into one table.
M = data.frame(month=1:12, Q_mean=round(Q_mean,3), Q_10, Q_90)

# Write to file.
write.table(M, "output/monthly_averages.dat", row.names=FALSE)






