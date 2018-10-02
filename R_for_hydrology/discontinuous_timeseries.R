

################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load package
library(zoo)


########
### Data
########

# Open and read NetCDF file (it will become a list)
d = read.table("data/NO3_conc.dat", header=T)


##############
### Processing
##############

# Transform date of discontinuous series (abbreviated as dc) into class date
date_dc = as.Date(d$date, format="%Y-%m-%d")

# Transform into zoo-object
NO3_dc = zoo(d$NO3, date_dc)

# Make a vector with all days between the first and last day and transform into zoo-object
t_range = zoo(NA, seq(from=date_dc[1], to=date_dc[length(date_dc)], by="day"))

# Combine the vector with all days and the zoo-object with date-measurement combinations
M = merge.zoo(t_range, NO3_dc)

# Extract date of continuous series
date_c = index(M)

# Rewrite to YYYMMDD (not strictly necessary)
date = as.numeric(paste0(substr(date_c,1,4),substr(date_c,6,7),substr(date_c,9,10)))

# Extract data
NO3 = data.frame(M)[,2]

# If you want, you can fill the gaps, but only do it if it makens sense
# (which in this case is not smart)
NO3 = round(na.approx(NO3),3)

# Combine dates and values
m = cbind(date, NO3)


##########
### Output
##########

# Write the result to file
write.table(m, "NO3_conc_continuous.dat", row.names=F)


