
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

# Read locations of nodes
d_h = read.table("data/Water depth at pressure points.xyz", header=T)

# Read size of nodes
d_A= read.table("data/cell_area.xyz", header=T)

# Extract water level
h = d_h[,3]

# Extract area of cells
A = d_A[,3]

# Extract locations of cells
x = d_A[,1]
y = d_A[,2]


##############
### Processing
##############

# Compute storage volume [m3]
V = h * A

# Compute total storage volume [m3]
V_total = sum(V)


##########
### Output
##########


# Open pdf
pdf("figs/D_Hydro_compute_storage.pdf", width=4, height=4, family="Times")

# Change margins (mar), distance of axes and labels (mgp), tick marks (tcl) 
# and number of subfigures (mfrow - 2 rows and 2 columns).
par(mar=c(2.1,2.1,0.1,0.1), mgp=c(1.2,0.2,0), tcl=-0.2, mfrow=c(1,1), pty="s")


# Plot storage volume per node on a map.
# The size of the marker scales with the storage.
plot(x, y, type="p", pch=16, cex=V/400, col="dodgerblue")

# Close pdf
graphics.off()

