
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

# read data
M = read.table("data/SS_par_GoF_MC_H.dat", header=TRUE)

# select variables to plot as x, y and z
M1 = data.frame(x=M$cW, y=M$cQ, z=M$NS)


##############
### Processing
##############

# Two interpolation functions:

# inverse distance interpolation in the plane : given the data
# to interpolate it returns a function that can be
# be used (as with approxfun)
# the argument tointerp should be a data frame that has
# a column called "x" and a column called "y"
# the name of the column to be interpolated can be selected
# by the argument zname
# the domain should be a polygon, outside of this the value NA will 
# be returned
# if the result function is called with two numbers a argument, an
# interpolated value will be returned
# if it is called with a vector for x and/or y a matrix will be returned
# that gives interpolated values for the grid defined by these vectors

invdist.fun  = function (tointerp,power=2,zname="z",domain=NULL) 
{
  foo = function(x,y)
  {
    w = 1/((tointerp$x-x)^2+(tointerp$y-y)^2)^(power/2)
    iexact = which(w==Inf)
    if(length(iexact)>0)
    {
      result=tointerp[iexact,zname]
    }
    else
    {
      result=sum(tointerp[,zname]*w)/sum(w)
    }
    if(!is.null(domain))
    {
      if(!GEO$inside.poly(domain,c(x,y)))
      {
        result=NA  
      }
    }
    return(result)
  }
  
  return(Vectorize(foo))
}


# nearest neighbour interpolation in the plane : given the data
# to interpolate it returns a function that can be
# be used (as with approxfun) and returns the value
# of the nearest point
# the argument tointerp should be a data frame that has
# a column called "x" and a column called "y"
# the name of the column to be interpolated can be selected
# by the argument zname
# the domain should be a polygon, outside of this the value NA will 
# be returned
# if the result function is called with two numbers a argument, an
# interpolated value will be returned
# if it is called with a vector for x and/or y a matrix will be returned
# that gives interpolated values for the grid defined by these vectors

nearestneighbour.fun = function(data,zname="z",domain=NULL)
{
  foo= function(x,y)
  {
    point = c(x,y)
    i = which.min((x-data[,"x"])^2+(y-data[,"y"])^2)
    result = data[i,zname]
    if(!is.null(domain))
    {
      if(!GEO$inside.poly(domain,point))
      {
        result=NA  
      }
    }   
    return(result)
  }
  return(Vectorize(foo))
}


# Set resolution of plot
Npl=50

# set plot range
xrange = seq(350, 480, length=Npl)
yrange = seq(0.1, 80, length=Npl)

# Interpolate between points (choose invdist.fun or nearestneighbour.fun)
F1 = invdist.fun(M1, power=0.5)
z1 = outer(xrange, yrange, F1)




##########
### Output
##########


# Define color range
col=colorRampPalette(c("lightcyan","lightblue","olivedrab1","yellow","orange","firebrick1","deeppink4"))(100)


# Start figure
pdf("figs/fit_surface.pdf", width=4, height=4, family="Times")
par(oma=c(0,2,0,0), mar=c(2.1,0.1,0.1,0.1), mfrow=c(1,1), mgp=c(1.2,0.2,0), tcl=-0.2, cex=1, pty="s")

# Color surface
image(x=xrange, y=yrange, z=z1, col=col, xlab="", ylab="", xaxt="n", yaxt="n")

# Add contour lines
contour(x=xrange, y=yrange, z1, add=TRUE, levels=seq(0,0.9,0.002))

# Add axes and labels
mtext(side=1, expression(paste("c"[W]," [mm]")), line=1.2)
mtext(side=2, expression(paste("c"[Q]," [h]")), line=1)
axis(side=1, at=c(360,380,400,420,440,460,480), labels=c("360","","400","","440","","480   "))
axis(side=2, at=seq(0,80,20), labels=c("0","20","40","60","80   "))
box()

# Close figure
graphics.off() 

