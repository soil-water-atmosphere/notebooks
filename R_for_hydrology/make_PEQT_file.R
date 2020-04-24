
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")


# load package for merging data
library(zoo)

# load package for fixed witdh output files
library(gdata)

# Set system time
Sys.setenv(TZ="UTC")
# NOTE: the argument tz="UTC" is really necessary - otherwise it adds daylight  
# savings time and time zone differences


########
### Data
########

### SPECIFY FOR CATCHMENT

# Closest automatic weather station (AWS)
KNMI  = "Hupsel" 

# Specify if manual gauges are used (TRUE) or only AWS (FALSE):
use_manual_gauges = TRUE

# If needed: manual KNMI rain gauges (make vector with all manual gauges)
gauges = c("Hupsel","Aalten")

# Weights of KNMI rain gauges
gauges_weight = c(0.8,0.2)

# Catchment name
Qstat = "TestRiver"       

# Catchment size (in km2)
size  = 18


### PREPARE DATA

# Step 1. Discharge data

# Make a txt file with hourly discharge data 
# and call it "Q_m3s_xx_hour.txt", where xx is replaced with the name of the station


# Step 2. KNMI data
 
# Go to 
# http://www.knmi.nl/nederland-nu/klimatologie-metingen-en-waarnemingen
# Click on "Uurwaarden van weerstations"
# to download hourly data of automatic weather stations
# (For map of stations, see: http://www.knmi.nl/klimatologie/images_algemeen/stations.jpg)
# Select columns T, Q and RH and the station and period of choice
# Click download dataset
# Save dataset in folder with discharge data with name KNMI_xx_hour.txt , 
# where xx is replaced with the name of the station
# Open this file in Notepad, remove the # before the line STN,YYYYMMDD,etc. and save
 
# Go to 
# http://www.knmi.nl/klimatologie/daggegevens/selectie.cgi 
# Click on "Dagwaarden van weerstations"
# (For map of stations, see: http://www.knmi.nl/klimatologie/images_algemeen/stations.jpg)
# Select column EV24 and the station and period of choice
# Click download dataset
# Save dataset in folder with discharge data with name KNMI_xx_day.txt , 
# where xx is replaced with the name of the station
# Open this file in Notepad, remove the # before the line STN,YYYYMMDD,etc. and save
 
# Go to 
# http://www.knmi.nl/klimatologie/monv/reeksen/select_rr.html 
# Click on "Dagwaarden neerslagstations"
# to download daily data manual rain gauges (if necessary)
# (For map of stations, see: http://www.knmi.nl/klimatologie/images_algemeen/stations_neerslag.png)
# Select station and period of choice (download files from different stations separately)
# Click download dataset
# Save dataset in folder with discharge data with name KNMI_xx_P.txt , where xx is replaced with the name of the station


### READ DATA

# Read hourly KNMI data
dh_all = read.table(paste0("data/KNMI_",KNMI,"_hour.txt"), header=TRUE, sep= ",")

# Read daily KNMI data
dd_all = read.table(paste0("data/KNMI_",KNMI,"_day.txt"), header=TRUE, sep=",")

# Read hourly discharge data (should be in m3/s)
dQ_all = read.table(paste0("data/Q_m3s_",Qstat,"_hour.txt"), header=TRUE)

# Read daily KNMI rain gauge data
if(use_manual_gauges==TRUE){
N = length(gauges)
for(i in 1:N){
  assign(paste("dP",i,"_all", sep=""), 
        read.table(paste0("data/KNMI_",gauges[i],"_P.txt"), header=TRUE, sep=",", skip=23))}}


##############
### Processing
##############

### SELECT OVERLAPPING PERIOD

# convert KNMI dates           
dh_all$date   = dh_all$YYYYMMDD *100 + dh_all$HH-1     # -1 is for 24 o'clock of KNMI-data

# determine ranges of gauges time series
range_gauges = matrix(ncol=2, nrow=N)
for(i in 1:N) {range_gauges[i,] = range(get(paste("dP",i,"_all", sep=""))$YYYYMMDD)*100}

# determine period of simultaneous measurements (only take whole days)
start = ceiling(max(dh_all$date[1]           , dd_all$YYYYMMDD[1]           *100, dQ_all$date[1]           , range_gauges[,1]) /100) *100
end   = floor  (min(dh_all$date[nrow(dh_all)], dd_all$YYYYMMDD[nrow(dd_all)]*100, dQ_all$date[nrow(dQ_all)], range_gauges[,2]) /100) *100
if(start>end){print("Q and KNMI data series don't overlap")}

# make series shorter
dh = dh_all[dh_all$date     >= start     & dh_all$date     < end    ,]
dd = dd_all[dd_all$YYYYMMDD >= start/100 & dd_all$YYYYMMDD < end/100,]
dQ = dQ_all[dQ_all$date     >= start     & dQ_all$date     < end    ,]
if(use_manual_gauges==TRUE){for(i in 1:N) {assign(paste0("dP",i), 
               get(paste0("dP",i,"_all"))[get(paste0("dP",i,"_all"))$YYYYMMDD >= start/100 & 
               get(paste0("dP",i,"_all"))$YYYYMMDD < end/100,])}}


### EXTRACT AND CONVERT DATA

# extract P from automatic weather station
P_AWS          = dh$RH /10  # divide by 10 to get mm/h
P_AWS[P_AWS<0] = 0

if(use_manual_gauges==TRUE){
  # compute catchment average P
  P_gauges = rep(0, nrow(dP1))
  for(i in 1:N) {P_gauges = P_gauges + get(paste("dP",i, sep=""))$RD /10 * gauges_weight[i]}
}else{
  
}

# extract T from automatic weather station
T = dh$T /10  # divide by 10 to get degrees Celsius

# convert Q data from m3/s to mm/h
Q = round(dQ$Q /(size*1e6) *3600 *1000 ,5)


# DIVIDE DAILY ETpot OVER HOURS

# ETpot data: distribute daily ET sum over hours using global radiation
# extract global radiation (note: this Q is not discharge but global radiation)
GloRad_hour = dh$Q 

# compute daily sum of global radiation 
agg         = aggregate(dh$Q, by=list(dh$YYYYMMDD), FUN=sum) 

# compute daily sum of global radiation 
GloRad_day  = rep(agg[,2], each=24)

# extract daily sum of ETpot
ETpot_day   = rep(dd$EV24 /10, each=24)

# compute hourly sums of ETpot
ETpot       = round(ETpot_day * GloRad_hour / GloRad_day , 3)


### DIVIDE DAILY P OVER HOURS

if(use_manual_gauges==TRUE){
  # Shift P time series 16 hours ahead, since total P at day 2 consists of data
  # from 8:00 (day 1) until 8:00 (day 2)
  
  # compute daily sum of precipitation at AWS
  days_shifted  = c(rep(0,8), dh$YYYYMMDD[1:(nrow(dh)-8)])
  agg           = aggregate(P_AWS, by=list(days_shifted), FUN=sum) 
  P_AWS_day     = rep(agg[2:(nrow(agg)-1),2], each=24)
  P_gauges_day  = rep(P_gauges[2:length(P_gauges)], each=24)
  
  # shift daily measurements back
  P_AWS_day    = c(rep(NA,8), P_AWS_day   , rep(NA,16))
  P_gauges_day = c(rep(NA,8), P_gauges_day, rep(NA,16))
  
  # compute hourly sums of P
  P = P_gauges_day * P_AWS / P_AWS_day
  P [P_AWS_day == 0] = 0
  P [P_AWS_day == 0 & is.na(P_gauges_day) == FALSE & P_gauges_day > 0] = 
    P_gauges_day [P_AWS_day == 0 & is.na(P_gauges_day) == FALSE & P_gauges_day > 0] /24
  P = round(P, 3)
}else{
  P = P_AWS
}

### COMBINE DATA

# combine into one time series
PET_zoo = zoo(cbind(P,ETpot,T), strptime(as.character(dh$date), format="%Y%m%d%H", tz="UTC"))
Q_zoo  = zoo(cbind(Q), strptime(as.character(dQ$date), format="%Y%m%d%H", tz="UTC"))

# make empty range (to make sure that all hours are in the data set)
# NOTE: the argument tz="UTC" is really necessary, otherwise it adds daylight savings time 
# and time zone differences. The as.POSIXlt is necessary too, because otherwise the data 
# are in POSIXlt and t_range in POSIXct.
t_range = zoo(NA, as.POSIXlt(seq(from = strptime(as.character(start),format="%Y%m%d", tz="UTC"),
                                 to   = strptime(as.character(end  ),format="%Y%m%d", tz="UTC"), by="hour") ))
# remove last element
t_range = t_range[1:(length(t_range)-1)]

# merge
M = merge.zoo(t_range, PET_zoo, Q_zoo)


### COMBINE DATA

# rewrite date as YYYYMMDDHH
d = as.character(index(M))
date = as.numeric(substr(d, 1 , 4 ))   * 1000000 +
       as.numeric(substr(d, 6 , 7 ))   * 10000   +
       as.numeric(substr(d, 9 , 10))   * 100     +
       as.numeric(substr(d, 12, 13)) 
m = cbind(date, data.frame(M)[,c(2,3,5,4)])


##########
### Output
##########

# write data to file
options(scipen=999) # to suppress e-4 notation in data files
write.fwf(m, paste0("output/PEQT_",Qstat,"_hour.dat"), na="NA")

# Use t he script plot_PEQ.R to visualise data       
            
            
            

