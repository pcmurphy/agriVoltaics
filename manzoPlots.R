####
#
# Data plotting script for GBG Manzo Elementary AgriVoltaics site
# Author: P. Murphy
# Email: murphyp@email.arizona.edu
# Date: 2019-01-16
# Revised: 2019-02-26
#
###


###
#
# This script is designed to generate rough plots of data from the Manzo AgriVoltaics site to 
# look for sensor malfunction, extreme events, and overall site status.
#
# Plots generated:
#   -VWC (Volumetric Water Content) of both the control and experiment (PV) gardens
#   -Soil Temperature of both the control and experiment (PV) gardens
#   -Micromet (Air Temperature and RH) of both the control and experiment (PV) gardens
#   -Panel temperature experiment (garden) panels
#
# Successful running of this script requires the following two variables be set:
collectionDate <- "2019_02_13"  # format collectionDate as "yyyy_mm_dd", otherwise the script won't work
plotDays <- 36  # define how many days prior to the collection date that you want to plot
#
# Save the script, and then source it:
#   -Click Source in RStudio
# OR
#   -In the console, run: source("manzoPlots.R")
#
# Errors will cause the script to fail, Warning messages indicate something wasn't quite right,
# but the script continued anyway.
#
###










#####
# Variables

# Static
if(.Platform$OS.type == "unix") {
  dataPath <- "/Volumes/shares/SGD_BIO2/Biosphere 2/B2 Solar Yard & Agrivoltaics/Agrivoltaics Campbell Datalogger/data/MANZO/"
} else {
  dataPath <- "//boom.sbs.arizona.edu/shares/SGD_BIO2/Biosphere 2/B2 Solar Yard & Agrivoltaics/Agrivoltaics Campbell Datalogger/data/MANZO/"
}

recentDate <- as.POSIXct(paste(substring(collectionDate,1,4),substring(collectionDate,6,7),substring(collectionDate,9,10), sep = "-"))
firstDate <- recentDate-(86400*plotDays)
fileDate <- paste0(substring(collectionDate,1,4),substring(collectionDate,6,7),substring(collectionDate,9,10))
dataFile <- paste0(dataPath,collectionDate,"/ManzoCR1000_Table1_",fileDate,".dat")

color4 <- c("blue","red","cadetblue2","orange")
color6 <- c("violet","blue","cadetblue2","red","orange","yellow")


##### 
# Functions
#source("/Users/pmurphy/Box Sync/R/toolbox/clear.R")

#####
# File control
dir.create(paste0("/Users/pmurphy/Box Sync/GBG/figures/manzoAgrivoltaics/",collectionDate))
setwd(paste0("/Users/pmurphy/Box Sync/GBG/figures/manzoAgrivoltaics/",collectionDate))


#####
# Import data
manzoColumns <- read.csv(file = dataFile, skip = 1)
manzoData <- read.csv(file = dataFile, skip = 3)
colnames(manzoData) <- colnames(manzoColumns)
rm(manzoColumns)


#####
# Process data

# Adjust time to POSIX format
manzoData$TIMESTAMP <- as.POSIXct(manzoData$TIMESTAMP, format="%Y-%m-%d %H:%M")

# Generate dataframe within specified dates specifically for plotting
manzoDataPlot <- manzoData[manzoData$TIMESTAMP<recentDate & manzoData$TIMESTAMP>firstDate,]

# Ensure columns are numeric format (often not the case if all NaN, will break plotting)
manzoDataPlot[,2:ncol(manzoDataPlot)] <- sapply(manzoDataPlot[,2:ncol(manzoDataPlot)],as.numeric)



#####
# Plot most recent data

# VWC
png(file = paste0("VWC_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$PV_VWC1,
     col = color6[1],
     type = "l",
     ylim = c(min(manzoDataPlot[,c("PV_VWC1","PV_VWC2","PV_VWC3","C_VWC1","C_VWC2","C_VWC3")], na.rm = T),max(manzoDataPlot[,c("PV_VWC1","PV_VWC2","PV_VWC3","C_VWC1","C_VWC2","C_VWC3")], na.rm = T)),
     #ylab = expression("VWC (m"^{3} ~ "m"^{-3}* ")"),
     ylab = "VWC (%)",
     xlab = "",
     xaxt = "n"
     )
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_VWC2,
      col = color6[2])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_VWC3,
      col = color6[3])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$C_VWC1,
      col = color6[4])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$C_VWC2,
      col = color6[5])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$C_VWC3,
      col = color6[6])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("PV1", "PV2", "PV3", "C1", "C2", "C3"),
       col=color6,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()

# soilT
png(file = paste0("soilT_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$PV_Temp1,
     col = color6[1],
     type = "l",
     ylim = c(min(manzoDataPlot[,c("C_Temp1","C_Temp2","C_Temp3")]),max(manzoDataPlot[,c("C_Temp1","C_Temp2","C_Temp3")])),
     ylab = expression("Soil Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Temp2,
      col = color6[2])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Temp3,
      col = color6[3])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$C_Temp1,
      col = color6[4])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$C_Temp2,
      col = color6[5])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$C_Temp3,
      col = color6[6])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("PV1", "PV2", "PV3", "C1", "C2", "C3"),
       col=color6,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# Micromet
png(file = paste0("met_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,4), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$AirTC_PV_Avg,
     col = color4[1],
     type = "l",
     ylim = c(min(manzoDataPlot[,c("AirTC_PV_Avg","AirTC_C_Avg")]),max(manzoDataPlot[,c("AirTC_PV_Avg","AirTC_C_Avg")])),
     ylab = expression("Air Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$AirTC_C_Avg,
      col = color4[2])
par(new = T)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$RH_PV,
     col = color4[3],
     type = "l",
     axes = F,
     xlab = NA,
     ylab = NA
     )
axis(side = 4)
mtext(side = 4, line = 3, "Relative Humidity (%)")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$RH_C,
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
par(mar=c(2,5,1,1), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$PAR_Den_PV_Avg,
     col = color4[1],
     type = "l",
     ylim = c(min(manzoDataPlot[,c("PAR_Den_PV_Avg","PAR_Den_C_Avg")]),max(manzoDataPlot[,c("PAR_Den_PV_Avg","PAR_Den_C_Avg")])),
     ylab = expression("PAR Density (" * mu * "mol"~ m^{-2} ~ s^{-1} * ")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PAR_Den_C_Avg,
      col = color4[2])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("PV","C"),
       col=color4,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# Solar Temp

# Garden
png(file = paste0("SolarTgarden_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$PV_Garden_1_Avg,
     col = color6[1],
     type = "l",
     ylim = c(0,120),
     ylab = expression("PV Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Garden_2_Avg,
      col = color6[2])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Garden_3_Avg,
      col = color6[3])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Garden_4_Avg,
      col = color6[4])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Garden_5_Avg,
      col = color6[5])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Garden_6_Avg,
      col = color6[6])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("Garden1", "Garden2", "Garden3", "Garden4", "Garden5", "Garden6"),
       col=color6,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# Classroom
png(file = paste0("SolarTclassroom_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$PV_Classroom_1_Avg,
     col = color6[1],
     type = "l",
     ylim = c(0,120),
     ylab = expression("PV Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Classroom_2_Avg,
      col = color6[2])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Classroom_3_Avg,
      col = color6[3])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Classroom_4_Avg,
      col = color6[4])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Classroom_5_Avg,
      col = color6[5])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Classroom_6_Avg,
      col = color6[6])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("Classroom1", "Classroom2", "Classroom3", "Classroom4", "Classroom5", "Classroom6"),
       col=color6,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()


# Turf
png(file = paste0("SolarTturf_",collectionDate,".png"), width = 2500, height = 1000, res = 300)
par(mar=c(2,5,1,1), lwd = 1)
plot(x = manzoDataPlot$TIMESTAMP,
     y = manzoDataPlot$PV_Turf_1_Avg,
     col = color6[1],
     type = "l",
     ylim = c(0,120),
     ylab = expression("PV Temp ("*degree*C*")"),
     xlab = "",
     xaxt = "n"
)
axis.POSIXct(1, at = seq(firstDate, recentDate, by = "5 days"), format = "%Y-%m-%d")
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Turf_2_Avg,
      col = color6[2])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Turf_3_Avg,
      col = color6[3])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Turf_4_Avg,
      col = color6[4])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Turf_5_Avg,
      col = color6[5])
lines(x = manzoDataPlot$TIMESTAMP,
      y = manzoDataPlot$PV_Turf_6_Avg,
      col = color6[6])
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("topleft",
       legend=c("Turf1", "Turf2", "Turf3", "Turf4", "Turf5", "Turf6"),
       col=color6,
       lty=1,
       cex=0.8)
box(lty = "solid", col = "black")
dev.off()














