
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
d = read.table("data/Q_TestRiver_day.txt", header=TRUE)


##############
### Processing
##############

# Extract the years from the date:
# Divide date-column by 10000 (to cut off the month and day) and round.
year  = d$date%/%10000

# Make vector with all years
years = min(year):max(year)

# Compute annual maxima:
# Make empty vector to be filled in for-loop.
am = c()
# Start for-loop.
for(i in 1:length(years)){
  # Check if there are not too many missing values in one year (here 30, but you can change that).
  if(sum(is.na(d$Q[year==years[i]])) < 30){
    # Take the maximum discharge in a certain year and put it on location i in the new vector.
    am[i] = max(d$Q[year==years[i]], na.rm=TRUE)
  }
# End for-loop.
}

# Sort annual maxima from high to low
Qsort = sort(am, decreasing=T)

# 
N = length(Qsort)

# Make vector for exceedance probability.
p = 1:N/(N+1)

# Transform regular x-axis to Gumbel reduced variate.
y = -log(-log(1-p))

# Fit line between Gumbel reduced variate and annual maximum discharge.
fit = lm(Qsort ~ y)

# Extract slope.
m = fit$coefficients[1]

# Extract intercept.
a = fit$coefficients[2]


##########
### Output
##########

### Plot Gumbel distribution

# Open pdf
pdf("figs/Gumbel_distribution.pdf", width=8, height=3)

# Make margins smaller
par(mar=c(4,4,0.5,1))

# For 1 row and 3 columns of figures
par(mfrow=c(1,3))

# Plot Gumbel reduced variate on the x-axis and discharge on the y-axis. 
plot(y, Qsort,
     xlab="-ln(-ln(1-p)) [-]", ylab="Q [mm/d]")
# Add fited Gumbel distribution 
# (note that x is whatever is on the x-axis, so in this case the Gumber reduced variate)
curve(m + a * x, add=TRUE, col="grey")

# Make a second plot with exceedance probability 0n the x-axis
plot(p, Qsort,
     xlab="Exceedance probability [-]", ylab="Q [mm/d]")
curve(m-a*log(-log(1-x)), add=TRUE, col="grey")

# Make a third plot with return period on the x-axis
plot(1/p, Qsort,
     xlab="Return period [y]", ylab="Q [mm/d]")
curve(m-a*log(-log(1-(1/x))), add=TRUE, col="grey")

# Clode pdf
graphics.off()



### Write output to file.

# Combine regression parameters into one table.
Mrp = data.frame(m, a)

# Write regression parameters to file.
write.table(Mrp, "output/Gumbel_distribution_regr_pars.dat", row.names=FALSE)

# Combine regression parameters into one table.
Mam = data.frame(year=years, Q=am)

# Write annual maxima to file.
write.table(Mam, "output/Gumbel_distribution_annual_maxima.dat", row.names=FALSE)

