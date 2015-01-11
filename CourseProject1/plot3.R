plot3 <- function() {
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
        
        png(filename = "plot3.png",width = 480,height = 480);
        plot(final$DateTime,final$Sub_metering_1,type="n",
             xlab = "",
             ylab = "Energy sub metering");
        lines(final$DateTime,final$Sub_metering_1,col="black",type="l");
        lines(final$DateTime,final$Sub_metering_2,col="red",type="l");
        lines(final$DateTime,final$Sub_metering_3,col="blue",type="l");
        legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               lwd = 1,
               col = c("black","red","blue"));
        dev.off();
}