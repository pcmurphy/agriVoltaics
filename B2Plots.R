####
#
# Data plotting script for GBG Biosphere 2 AgriVoltaics site
# Author: P. Murphy
# Email: murphyp@email.arizona.edu
# Date: 2019-01-21
# Revised: 2019-02-26
#
###


###
#
# This script is designed to generate rough plots of data from the B2 AgriVoltaics site to 
# look for sensor malfunction, extreme events, and overall site status.
#
# Plots generated:
#   -VWC (Volumetric Water Content) of both the control and experiment (PV) gardens
#   -Soil Temperature of both the control and experiment (PV) gardens
#   -Micromet (Air Temperature and RH) of both the control and experiment (PV) gardens
#   -Panel temperature of both the control (no garden) and experiment (garden) panels
#
# Successful running of this script requires the following two variables be set:
collectionDate <- "2019_01_30"  # format collectionDate as "yyyy_mm_dd", otherwise the script won't work
plotDays <- 16  # define how many days prior to the collection date that you want to plot
#
# Save the script, and then source it:
#   -Click Source in RStudio
# OR
#   -In the console, run: source("B2Plots.R")
#
# Errors will cause the script to fail, Warning messages indicate something wasn't quite right,
# but the script continued anyway.
#
###

















#####
# Variables
#

# Static
if(.Platform$OS.type == "unix") {
  dataPath <- "/Volumes/shares/SGD_BIO2/Biosphere 2/B2 Solar Yard & Agrivoltaics/Agrivoltaics Campbell Datalogger/data/manual downloads/"
} else {
  dataPath <- "//boom.sbs.arizona.edu/shares/SGD_BIO2/Biosphere 2/B2 Solar Yard & Agrivoltaics/Agrivoltaics Campbell Datalogger/data/manual downloads/"
}

recentDate <- as.POSIXct(paste(substring(collectionDate,1,4),substring(collectionDate,6,7),substring(collectionDate,9,10), sep = "-"))
firstDate <- recentDate-(86400*plotDays)
fileDate <- paste0(substring(collectionDate,1,4),substring(collectionDate,6,7),substring(collectionDate,9,10))
dataFileC <- paste0(dataPath,collectionDate,"/B2_Ag_C_CR1000_Table1_",fileDate,".dat")
dataFilePV <- paste0(dataPath,collectionDate,"/B2_Ag_PV_CR1000_Table1_",fileDate,".dat")
dataFilePVcontrol <- paste0(dataPath,collectionDate,"/B2_Ag_PVcontrol_CR1000_Table1_",fileDate,".dat")

color4 <- c("blue","red","cadetblue2","orange")
color24 <- c("Black",rgb(103,0,31, max = 255),rgb(214,96,77, max = 255),rgb(244,165,130, max = 255),rgb(253,219,199, max = 255),rgb(247,247,247, max = 255),rgb(209,229,240, max = 255),rgb(146,197,222, max = 255),rgb(67,147,195, max = 255),rgb(33,102,172, max = 255),rgb(5,48,97, max = 255),rgb(103,0,31, max = 255))


##### 
# Functions
#
#source("/Users/pmurphy/Box Sync/R/toolbox/clear.R")



#####
# File control
dir.create(paste0("/Users/pmurphy/Box Sync/GBG/figures/B2Agrivoltaics/",collectionDate))
setwd(paste0("/Users/pmurphy/Box Sync/GBG/figures/B2Agrivoltaics/",collectionDate))


#####
# Import data
B2ColumnsC <- read.csv(file = dataFileC, skip = 1)
B2DataC <- read.csv(file = dataFileC, skip = 3)
colnames(B2DataC) <- colnames(B2ColumnsC)
rm(B2ColumnsC)

B2ColumnsPV <- read.csv(file = dataFilePV, skip = 1)
B2DataPV <- read.csv(file = dataFilePV, skip = 3)
colnames(B2DataPV) <- colnames(B2ColumnsPV)
rm(B2ColumnsPV)

B2ColumnsPVcontrol <- read.csv(file = dataFilePVcontrol, skip = 1)
B2DataPVcontrol <- read.csv(file = dataFilePVcontrol, skip = 3)
colnames(B2DataPVcontrol) <- colnames(B2ColumnsPVcontrol)
rm(B2ColumnsPVcontrol)


#####
# Process data
#

# Adjust time to POSIX format
B2DataC$TIMESTAMP <- as.POSIXct(B2DataC$TIMESTAMP, format="%Y-%m-%d %H:%M")
B2DataPV$TIMESTAMP <- as.POSIXct(B2DataPV$TIMESTAMP, format="%Y-%m-%d %H:%M")
B2DataPVcontrol$TIMESTAMP <- as.POSIXct(B2DataPVcontrol$TIMESTAMP, format="%Y-%m-%d %H:%M")

# Generate dataframes within specified dates specifically for plotting
B2DataPlotC <- B2DataC[B2DataC$TIMESTAMP<recentDate & B2DataC$TIMESTAMP>firstDate,]
B2DataPlotPV <- B2DataPV[B2DataPV$TIMESTAMP<recentDate & B2DataPV$TIMESTAMP>firstDate,]
B2DataPlotPVcontrol <- B2DataPVcontrol[B2DataPVcontrol$TIMESTAMP<recentDate & B2DataPVcontrol$TIMESTAMP>firstDate,]

# Ensure columns are numeric format (often not the case if all NaN, will break plotting)
B2DataPlotC[,2:ncol(B2DataPlotC)] <- sapply(B2DataPlotC[,2:ncol(B2DataPlotC)],as.numeric)
B2DataPlotPV[,2:ncol(B2DataPlotPV)] <- sapply(B2DataPlotPV[,2:ncol(B2DataPlotPV)],as.numeric)
B2DataPlotPVcontrol[,2:ncol(B2DataPlotPVcontrol)] <- sapply(B2DataPlotPVcontrol[,2:ncol(B2DataPlotPVcontrol)],as.numeric)



#####
# Plot most recent data
#

# VWC

# PV garden
png(file = paste0("VWC_PVgarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd=1)
plot(x = B2DataPlotPV$TIMESTAMP,
     y = B2DataPlotPV$VWC.1.,
     col = color24[1],
     type = "l",
     ylim = c(0,0.6),
     ylab = expression("VWC (m"^{3} ~ "m"^{-3}* ")"),
     xlab = "",
     xaxt = "n"
     )
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
for (i in c(2:12)) {
  plotNum <- paste0("VWC.",i,".")
  
  lines(x = B2DataPlotPV$TIMESTAMP,
        y = B2DataPlotPV[,plotNum],
        col = color24[i])
}
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("bottomleft",
       legend=paste0("PV",seq(1:12)),
       col=color24,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()

# Control garden
png(file = paste0("VWC_Cgarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd=1)
plot(x = B2DataPlotC$TIMESTAMP,
     y = B2DataPlotC$VWC.1.,
     col = color24[1],
     type = "l",
     ylim = c(0,0.6),
     ylab = expression("VWC (m"^{3} ~ "m"^{-3}* ")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
for (i in c(2:12)) {
  plotNum <- paste0("VWC.",i,".")
  
  lines(x = B2DataPlotC$TIMESTAMP,
        y = B2DataPlotC[,plotNum],
        col = color24[i])
}
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("bottomleft",
       legend=paste0("C",seq(1:12)),
       col=color24,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# soilT

# PV garden
png(file = paste0("SoilT_PVgarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd=1)
plot(x = B2DataPlotPV$TIMESTAMP,
     y = B2DataPlotPV$Temp.1.,
     col = color24[1],
     type = "l",
     ylim = c(0,50),
     ylab = expression("Soil Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
for (i in c(2:12)) {
  plotNum <- paste0("Temp.",i,".")
  
  lines(x = B2DataPlotPV$TIMESTAMP,
        y = B2DataPlotPV[,plotNum],
        col = color24[i])
}
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("bottomleft",
       legend=paste0("PV",seq(1:12)),
       col=color24,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()

# Control garden
png(file = paste0("SoilT_Cgarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd=1)
plot(x = B2DataPlotC$TIMESTAMP,
     y = B2DataPlotC$Temp.1.,
     col = color24[1],
     type = "l",
     ylim = c(0,50),
     ylab = expression("Soil Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
for (i in c(2:12)) {
  plotNum <- paste0("Temp.",i,".")
  
  lines(x = B2DataPlotC$TIMESTAMP,
        y = B2DataPlotC[,plotNum],
        col = color24[i])
}
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("bottomleft",
       legend=paste0("C",seq(1:12)),
       col=color24,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# Micromet
png(file = paste0("met_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(4,5,1,4), lwd=1)
plot(x = B2DataPlotPV$TIMESTAMP,
     y = B2DataPlotPV$AirTC_PV1_Avg,
     col = color4[1],
     type = "l",
     ylim = c(-10,50),
     ylab = expression("Air Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = B2DataPlotC$TIMESTAMP,
      y = B2DataPlotC$AirTC_C_Avg,
      col = color4[2])
par(new = T)
plot(x = B2DataPlotPV$TIMESTAMP,
     y = B2DataPlotPV$RH_PV1,
     col = color4[3],
     type = "l",
     axes = F,
     xlab = NA,
     ylab = NA
     )
axis(side = 4)
mtext(side = 4, line = 3, "Relative Humidity (%)")
lines(x = B2DataPlotC$TIMESTAMP,
      y = B2DataPlotC$RH_C,
      col = color4[4])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("AirT PV","AirT C","RH PV","RH C"),
       col=color4,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# PAR
png(file = paste0("PAR_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(4,5,1,1), lwd=1)
plot(x = B2DataPlotPV$TIMESTAMP,
     y = B2DataPlotPV$PAR_Den_PV1_Avg,
     col = color4[1],
     type = "l",
     ylim = c(min(B2DataPlotC[,"PAR_Den_C_Avg"], na.rm = T),max(B2DataPlotC[,c("PAR_Den_C_Avg")], na.rm = T)),
     ylab = expression("PAR Density (" * mu * "mol"~ m^{-2} ~ s^{-1} * ")"),
     xlab = "Date"#,
     #xaxt = "n"
)
#axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = B2DataPlotPV$TIMESTAMP,
      y = B2DataPlotPV$PAR_Den_PV2_Avg,
      col = color4[3])
lines(x = B2DataPlotC$TIMESTAMP,
      y = B2DataPlotC$PAR_Den_C_Avg,
      col = color4[2])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("PV1","PV2","C"),
       col=c(color4[1],color4[3],color4[2]),
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()



# Solar panel temp

# PV garden
png(file = paste0("solarT_PVgarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd=1)
plot(x = B2DataPlotPV$TIMESTAMP,
     y = B2DataPlotPV$SEVolt_Avg.1.,
     col = color24[1],
     type = "l",
     ylim = c(0,200),
     ylab = expression("PV Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
for (i in c(2:24)) {
  plotNum <- paste0("SEVolt_Avg.",i,".")
  
  lines(x = B2DataPlotPV$TIMESTAMP,
        y = B2DataPlotPV[,plotNum],
        col = color24[i])
}
# grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
#      lwd = par("lwd"), equilogs = TRUE)
# legend("bottomleft",
#        legend=paste0("Garden",seq(1:24)),
#        col=color24,
#        lty=1,
#        cex=0.8)
# box(lty = "solid", col = "black")
dev.off()

# Control garden
png(file = paste0("solarT_controlGarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd=1)
plot(x = B2DataPlotPVcontrol$TIMESTAMP,
     y = B2DataPlotPVcontrol$SEVolt_Avg.1.,
     col = color24[1],
     type = "l",
     ylim = c(0,200),
     ylab = expression("PV Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
for (i in c(2:24)) {
  plotNum <- paste0("SEVolt_Avg.",i,".")
  
  lines(x = B2DataPlotPVcontrol$TIMESTAMP,
        y = B2DataPlotPVcontrol[,plotNum],
        col = color24[i])
}
# grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
#      lwd = par("lwd"), equilogs = TRUE)
# legend("bottomleft",
#        legend=paste0("Garden",seq(1:24)),
#        col=color24,
#        lty=1,
#        cex=0.8)
box(lty = "solid", col = "black")
dev.off()














