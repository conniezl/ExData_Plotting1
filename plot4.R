# This function Plots graphics #4 
# Notes: 
#   1. This script needs to be loaded in the working folder. 
#   2. Memory space of at least 120MB needed.
#   3. Requires reshape2 library to use melt function. 

# Usage:
#   Source("plot4.R")
#   plot <- plot4()
# input: The household_power_consumption.txt file must exist in the working directory (extracted from zip file).
# output: A PNG file  480 x 480 pixels with the plot(s)


plot4 <- function() {
  
  ePower <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE)
  
  # Get only the rows needed: Feb 1 and 2 2007 
  
  ePowerTwoDays <- ePower[grepl("^1/2/2007|^2/2/2007",ePower$Date),]
  ePowerTwoDays$dateTime<-paste(ePowerTwoDays$Date,ePowerTwoDays$Time, sep=" ")
  
  # Ensure the metrics have a numeric class. Suppress the warning displayed for the '?' that are coerced  to NA's
  
  ePowerTwoDays$Global_active_power <- suppressWarnings(as.numeric(ePowerTwoDays$Global_active_power))
  ePowerTwoDays$Global_reactive_power <- suppressWarnings(as.numeric(ePowerTwoDays$Global_reactive_power))
  ePowerTwoDays$Voltage <- suppressWarnings(as.numeric(ePowerTwoDays$Voltage))
  ePowerTwoDays$Global_intensity <- suppressWarnings(as.numeric(ePowerTwoDays$Global_intensity))
  ePowerTwoDays$Sub_metering_1 <- suppressWarnings(as.numeric(ePowerTwoDays$Sub_metering_1))
  ePowerTwoDays$Sub_metering_2 <- suppressWarnings(as.numeric(ePowerTwoDays$Sub_metering_2))
  ePowerTwoDays$Sub_metering_3 <- suppressWarnings(as.numeric(ePowerTwoDays$Sub_metering_3))
  

  # Create a separate data frame only for the sub_metering plot, because this dataframe will be melt.
  # Other plots will be drawn from the existing data frame
  
  ePowerSubmetering <- ePowerTwoDays[,c(10,7,8,9)]  # Create a data frame containing only columns datetime and the sub_metering measures
  require (reshape2)
  ePowerSubmetering<-melt(ePowerSubmetering,id="dateTime", measure.vars=c("Sub_metering_1","Sub_metering_2","Sub_metering_3" ),na.rm=TRUE)
  
  # Convert character datetime to proper Datetime class

  ePowerSubmetering$dateTime <- strptime(ePowerSubmetering$dateTime,format="%d/%m/%Y %H:%M:%S")
  ePowerTwoDays$dateTime <- strptime(ePowerTwoDays$dateTime,format="%d/%m/%Y %H:%M:%S")
  
  # Create plots and save to png in the working directory
  png(file = "./plot4.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white")

  par(mfrow = c(2,2))
  plot(ePowerTwoDays$dateTime,ePowerTwoDays$Global_active_power,type="l",xlab="",ylab="Global Active Power")
  plot(ePowerTwoDays$dateTime,ePowerTwoDays$Voltage,type="l",xlab="datetime",ylab="Voltage")
  with (ePowerSubmetering, 
        {   plot(dateTime, value, type = "n", xlab="", ylab="Energy sub metering")
            lines(dateTime[variable == "Sub_metering_1"], value[variable == "Sub_metering_1"], col="black")
            lines(dateTime[variable == "Sub_metering_2"], value[variable == "Sub_metering_2"], col="brown1")
            lines(dateTime[variable == "Sub_metering_3"], value[variable == "Sub_metering_3"], col="blue")
            legend("topright", lty=1, lwd=2 ,col=c("black","brown1","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), box.lty=0)
            box(lty=1)
        })

  plot(ePowerTwoDays$dateTime,ePowerTwoDays$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power")
  
  dev.off()
  
  
}