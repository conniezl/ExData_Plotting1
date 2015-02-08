# This function Plots graphic #3 
# Notes: 
#   1. This script needs to be loaded in the working folder. 
#   2. Memory space of at least 120MB needed.
#   3. Requires reshape2 library to use melt function. 

# Usage:
#   Source("plot3.R")
#   plot <- plot3()
# input: The household_power_consumption.txt file must exist in the working directory (extracted from zip file).
# output: A PNG file  480 x 480 pixels with the plot


plot3 <- function() {
  
  ePower <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE)
  
  # Get only the rows and columns needed: Feb 1 and 2 2007 dates and times, and the Sub_metering 3 values. Add a new column with date+time.
  
  ePowerTwoDays <- ePower[grepl("^1/2/2007|^2/2/2007",ePower$Date),c(1,2,7,8,9)]
  ePowerTwoDays$dateTime<-paste(ePowerTwoDays$Date,ePowerTwoDays$Time, sep=" ")
  
  # Ensure the metrics have a numeric class. Suppress the warning displayed for the '?' that are coerced  to NA's
  # Note: to display the '?' in the data use : ePowerTwoDays[grepl("\\?",ePowerTwoDays$Sub_metering_1) | grepl("\\?",ePowerTwoDays$Sub_metering_2),]
  
  ePowerTwoDays$Sub_metering_1 <- suppressWarnings(as.numeric(ePowerTwoDays$Sub_metering_1))
  ePowerTwoDays$Sub_metering_2 <- suppressWarnings(as.numeric(ePowerTwoDays$Sub_metering_2))
  ePowerTwoDays$Sub_metering_3 <- suppressWarnings(as.numeric(ePowerTwoDays$Sub_metering_3))

  
  # melt the data frame to have the sub_metering measures as one column 
  
  require (reshape2)
  ePowerTwoDays<-melt(ePowerTwoDays,id="dateTime", measure.vars=c("Sub_metering_1","Sub_metering_2","Sub_metering_3" ),na.rm=TRUE)

  # Convert character to Datetime class
  ePowerTwoDays$dateTime <- strptime(ePowerTwoDays$dateTime,format="%d/%m/%Y %H:%M:%S")
  
  # Create plot with lines and save to png in the working directory
  png(file = "./plot3.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white")

  plot(ePowerTwoDays$dateTime, ePowerTwoDays$value, type = "n", xlab="", ylab="Energy sub metering")
  lines(ePowerTwoDays$dateTime[ePowerTwoDays$variable == "Sub_metering_1"], ePowerTwoDays$value[ePowerTwoDays$variable == "Sub_metering_1"], col="black")
  lines(ePowerTwoDays$dateTime[ePowerTwoDays$variable == "Sub_metering_2"], ePowerTwoDays$value[ePowerTwoDays$variable == "Sub_metering_2"], col="brown1")
  lines(ePowerTwoDays$dateTime[ePowerTwoDays$variable == "Sub_metering_3"], ePowerTwoDays$value[ePowerTwoDays$variable == "Sub_metering_3"], col="blue")
  legend("topright", lty=1, lwd=2 ,col=c("black","brown1","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

  dev.off()
  
  
}