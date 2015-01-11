plot4 <- function() {
        bigdata <- read.csv('household_power_consumption.txt',sep=";",na.strings="?");
        bigdata$Date <- as.Date(bigdata$Date, "%d/%m/%Y");
        subset <- bigdata[bigdata$Date == "2007-02-01" | bigdata$Date == "2007-02-02",];
        rm(bigdata);
        convert1 <- lapply(subset[,3:9],as.character);
        convert2 <- lapply(convert1,as.numeric);
        final <- cbind(subset[,1:2],convert2);
        
        DateTime <- paste(final$Date,final$Time);
        DateTime <- strptime(DateTime, "%Y-%m-%d %H:%M:%S");
        final <- cbind(final,DateTime);
        
        png(filename = "plot4.png",width = 480,height = 480);
        par(mfrow=c(2,2))
        with(final, {
                plot(final$DateTime,final$Global_active_power,
                     type = "l",
                     xlab = "",
                     ylab = "Global Active Power")
                plot(final$DateTime,final$Voltage,
                     type = "l",
                     xlab = "datetime",
                     ylab = "Voltage")
                plot(final$DateTime,final$Sub_metering_1,type="n",
                     xlab = "",
                     ylab = "Energy sub metering");
                lines(final$DateTime,final$Sub_metering_1,col="black",type="l")
                lines(final$DateTime,final$Sub_metering_2,col="red",type="l")
                lines(final$DateTime,final$Sub_metering_3,col="blue",type="l")
                legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                       lwd = 1,
                       col = c("black","red","blue"),
                       bty = "n")
                plot(final$DateTime,final$Global_reactive_power,
                     type = "l",
                     xlab = "datetime",
                     ylab = "Global_reactive_power")
                
        })
        dev.off();
}