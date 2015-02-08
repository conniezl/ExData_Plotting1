# This function Plots graphic #1 
# Notes: 
#   1. This script needs to be loaded in the working folder. 
#   2. Memory space of at least 120MB needed.

# Usage:
#   Source("plot1.R")
#   plot <- plot1()
# input: The household_power_consumption.txt file must exist in the working directory (extracted from zip file).
# output: A PNG file  480 x 480 pixels with the plot


plot1 <- function() {
  
  ePower <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE)
  
  # Get only the rows and columns needed: Feb 1 and 2 2007 dates and times, and the measure Global_active_power. 
  
  ePowerTwoDays <- ePower[grepl("^1/2/2007|^2/2/2007",ePower$Date),c(1,2,3)]

  
  # Ensure the metric has a numeric class. Suppress the warning displayed for the '?' that are coerced  to NA's if any.
  # Note: to display the '?' in the data use : ePowerTwoDays[grepl("\\?",ePowerTwoDays$Global_active_power) ,]
  
  ePowerTwoDays$Global_active_power <- suppressWarnings(as.numeric(ePowerTwoDays$Global_active_power))
  
  # Create histogram and save to png in the working directory
  png(file = "./plot1.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white")
  hist(ePowerTwoDays$Global_active_power, xlab="Global Active Power (kilowatts)", main="Global Active Power", col="red")
  dev.off()
  
  
}




