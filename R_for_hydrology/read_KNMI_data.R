
################
### Initializing
################

# Clear memory
rm(list=ls())

# Set working directory (change to your own location)
setwd("D:/education/R/example_scripts/")

# Load packages
library(zoo)


########
### Data
########

# First go to www.knmi.nl/klimatologie -> Metingen en Waarnemingen -> Dagwaarden van weertations
# Dowload the data fro De Bilt and unzip. The resulting fle is called etmgeg_260.txt.
# Open the file in notepad, uncomment the line starting with "# STN,YYYYMMDD,..."
# and remove the white line below that.

# Read KNMI data
d = read.table("data/etmgeg_260.txt", header=T, sep=",", skip=47)

# Extract the year (first 4 digits of YYYYMMDD).
# The command %/% takes the number before the decimal point after a division.
Y = d$YYYYMMDD%/%10000

# Define current year
now = max(Y)

# Make vector of all years
y = 1960:now


##############
### Processing
##############

# Make a matrix with empty vectors for number of days with maximum temperature 
# or precipitation sum above a certain value.
M = matrix(nrow=length(y), ncol=6)

# Run over all years to count instances.
for(i in 1:length(y)){
  M[i,1] = length(which(d$TX[Y==y[i]] > 250)) # T > 25 deg
  M[i,2] = length(which(d$TX[Y==y[i]] > 300)) # T > 30 deg
  M[i,3] = length(which(d$TX[Y==y[i]] > 350)) # T > 35 deg
  M[i,4] = length(which(d$RH[Y==y[i]] > 100)) # P > 10 mm
  M[i,5] = length(which(d$RH[Y==y[i]] > 250)) # P > 25 mm
  M[i,6] = length(which(d$RH[Y==y[i]] > 500)) # P > 50 mm
}

# Make matrices with occurrences.
Trange = seq(25,40,1)
Prange = seq(10,65,5)
MT = matrix(ncol=length(y), nrow=length(Trange))
MP = matrix(ncol=length(y), nrow=length(Prange))

# Run over all years.
for(i in 1:length(y)){
  # Run over all temperature thresholds.
  for(j in 1:nrow(MT)){
    MT[j,i] = length(which(d$TX[Y==y[i]] > Trange[j]*10))
  }
  # Run over all precipitation thresholds.
  for(j in 1:nrow(MP)){
    MP[j,i] = length(which(d$RH[Y==y[i]] > Prange[j]*10))
  }
}


##########
### Output
##########


# Define colors for the points and image
cols    = c("orange","red","black")
divs    = c(0,1,2,5,10,20,50,100)
coldivs = c("white",colorRampPalette(c("lightblue","dodgerblue","black"))(length(divs)-2))
xlabels = seq(1960,2020,5)

# Open pdf
pdf("figs/read_KNMI_data.pdf", width=8, height=8, family="Times")

# Change margins (mar), distance of axes and labels (mgp), tick marks (tcl) 
# and number of subfigures (mfrow - 2 rows and 2 columns).
par(mar=c(2.3,2.15,0.1,0.1), mgp=c(1.2,0.2,0), tcl=-0.2, mfrow=c(2,2), pty="s")


### Figure 1 and 2 (top)

for(i in 1:2){
  
  # For full control of the layout, first make an empty plot with nice axes.
  plot(y, M[,1], ylim=c(0,max(if(i==1){M[,1:3]}else{M[,4:6]})*1.02), type="n", yaxs="i",
       xlab="", ylab=if(i==1){"Number of days with max. temp. > x deg. C"}else{"Number of days with P sum > x mm"})
  grid()
  
  # Add points
  points(y, M[,(i-1)*3+1], col=cols[1], pch=16)
  points(y, M[,(i-1)*3+2], col=cols[2], pch=16)
  points(y[M[,(i-1)*3+3]>0], M[,(i-1)*3+3][M[,(i-1)*3+3]>0], col=cols[3], pch=16) # only the ones with occasions
  
  # Add moving averages
  lines(y, rollmean(M[,(i-1)*3+1],10,na.pad=T), col=cols[1])
  lines(y, rollmean(M[,(i-1)*3+2],10,na.pad=T), col=cols[2])
  
  # Add arrows for the current year
  arrows(now, M[,(i-1)*3+1][y==now], now, M[,(i-1)*3+1][y==now]+4 , col=cols[1], length=0.1)
  arrows(now, M[,(i-1)*3+2][y==now], now, M[,(i-1)*3+2][y==now]+3 , col=cols[2], length=0.1)
  arrows(now, M[,(i-1)*3+3][y==now], now, M[,(i-1)*3+3][y==now]+2 , col=cols[3], length=0.1)
  
  # Add legend
  legend(col=c(cols,"grey"), if(i==1){c("> 25 C","> 30 C","> 35 C","10-year
mov.ave.")}else{c("> 10 mm","> 25 mm","> 50 mm","10-year
mov.ave.")}, 
         x="topleft", bty="n", pch=c(16,16,16,NA), lty=c(NA,NA,NA,1), cex=0.8)
  
  # Add grey text on top
  mtext("De Bilt", col="grey", line=-1.5, side=3, cex=0.8)

}


### Figures 3 and 4 (bottom)

for(i in 1:2){
  
  # Image
  image(if(i==1){t(MT)}else{t(MP)}, breaks=divs, col=coldivs,
        xaxt="n", yaxt="n")
  
  # Add axes
  axis(side=1, at=(y[seq(1,60,5)]-y[1])/(y[length(y)]-y[1]), labels=y[seq(1,60,5)])
  if(i==1){r=Trange}else{r=Prange}
  axis(side=2, at=(r-r[1])/(r[length(r)]-r[1]), labels=r)
  mtext(side=2, line=1.2, if(i==1){"Daily max. temp."}else{"Daily P [mm]"}, cex=0.8)
  
  # Redraw black box
  box()
  
  # Add grey text on top
  mtext("De Bilt", col="grey", line=-1.5, side=3, cex=0.8)
  
  # Add legend
  text(x=0.01, y=0.95,expression("Number of
occurrences">=""), adj=c(0,0), cex=0.8)
  legend(x=0.01, y=0.95, legend=divs[2:(length(divs)-1)], col=coldivs[2:length(coldivs)], 
         pch=15, bty="n", cex=0.8)
}

# Close pdf
graphics.off()
