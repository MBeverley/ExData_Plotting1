
# Exploratory Data Analysis - Week 1 Project 

library(dplyr)
library(lubridate)


#Download the file

fileURL <-"https:/d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"   
if (!file.exists(destfile)) {
  download.file(fileURL ,"Electric Power Consumption.zip",method="auto") }


powerdata <- read.table(unz("Electric Power consumption.zip", "household_power_consumption.txt"), sep=";", header=TRUE)



#combine date and time variables
powerdata$DateTime <- paste(powerdata$Date, powerdata$Time)

powerdata$DateTime <- as_datetime(powerdata$DateTime, format= "%d/%m/%Y %H:%M:%S")

# filter out unnecessary observations
powerdata <- powerdata %>% 
  filter(DateTime <= as_datetime("2007-02-03") & DateTime >= as_datetime("2007-02-01"))


#convert variables to numeric 
varlist <- powerdata %>%
  select(Global_active_power:Sub_metering_3) %>%
  names()


  # Loop to ensure there are no "?" in the subset of data we have
  # Can disregard- not important for assignment
        a <- c()
        j = 0 # variable for the iteration of the variable loop
        sumlist <- numeric()
        for (vars in varlist) { 
        
                for (i in 1:length(powerdata[,vars])) { 
                         a[i] <- (isTRUE(powerdata[i,vars]=="?")) #a is a logical vector that is TRUE if observation i of variable vars is "?"
                         
                }
          
           j <- j+1 # j is the iteration of the varlist loop we are on (1-7)
           sumlist[j] = sum(a) # sumlist is a vector containing the sum of a (i.e., incidences of "?") in each variable
        }
        
        
        
        if(!isTRUE(sum(sumlist)==0)) {
          stop("? values detected")
        }
            

powerdata <- powerdata %>%
  mutate(across(all_of(varlist),as.numeric))

# Create Plot 1 
hist(powerdata$Global_active_power, main="Global Active Power", xlab="Frequency", 
     ylab ="Global Active Power (kilowatts)", col = "red")
dev.copy(png, file="plot1.png")
dev.off()

#Create Plot 2 


#create vector of days to represent on graph

 d1 <- as_datetime("2007-02-01 00:00:00")
 d2 <- as_datetime("2007-02-02 00:00:00")
 d3 <- as_datetime("2007-02-03 00:00:00")

d <- c(d1, d2, d3)


with(powerdata, plot(DateTime,Global_active_power, type="l", ylab = "Global Active Power (kilowatts)",
     xaxt="n", xlab=""))
axis(side=1, at=d, labels=format(d,'%a'))
dev.copy(png, file="plot2.png")
dev.off()

# Create Plot 3 
with(powerdata, plot(DateTime, Sub_metering_1, type="l", ylab="Energy sub metering", 
  xlab="", xaxt="n"))
  lines(powerdata$DateTime, powerdata$Sub_metering_2, col="red")
  lines(powerdata$DateTime, powerdata$Sub_metering_3, col="blue")
  axis(side=1, at=d, labels=format(d,'%a'))
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
         col=c("black","red","blue"), lty=1)

  dev.copy(png, file="plot3.png")
  dev.off()
  
# Create Plot 4 
  par(mfrow =c(2,2))
  
  #top left chart
 with(powerdata, plot(DateTime,Global_active_power, type="l", ylab = "Global Active Power (kilowatts)",
                       xaxt="n", xlab=""))
 axis(side=1, at=d, labels=format(d,'%a'))

  #top right chart
 with(powerdata, plot(DateTime,Voltage, type="l", ylab = "Voltage",
                      xaxt="n", xlab="datetime"))
 axis(side=1, at=d, labels=format(d,'%a'))
 
  
  #bottom left chart
 with(powerdata, plot(DateTime, Sub_metering_1, type="l", ylab="Energy sub metering", 
                      xlab="", xaxt="n"))
 lines(powerdata$DateTime, powerdata$Sub_metering_2, col="red")
 lines(powerdata$DateTime, powerdata$Sub_metering_3, col="blue")
 axis(side=1, at=d, labels=format(d,'%a'))
 legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
        col=c("black","red","blue"), lty=1, cex=0.4)
  
  #bottom right chart
 with(powerdata, plot(DateTime,Global_reactive_power, type="l",
                      xaxt="n", xlab="datetime"))
 axis(side=1, at=d, labels=format(d,'%a'))
 
 dev.copy(png, file="plot4.png")
 dev.off()
  
  
  
  