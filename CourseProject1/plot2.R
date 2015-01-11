plot2 <- function() {
        bigdata <- read.csv('household_power_consumption.txt',sep=";",na.strings="?");
        bigdata$Date <- as.Date(bigdata$Date, "%d/%m/%Y");
        subset <- bigdata[bigdata$Date == "2007-02-01" | bigdata$Date == "2007-02-02",];
        rm(bigdata)
        convert1 <- lapply(subset[,3:9],as.character);
        convert2 <- lapply(convert1,as.numeric);
        final <- cbind(subset[,1:2],convert2);
        
        DateTime <- paste(final$Date,final$Time);
        DateTime <- strptime(DateTime, "%Y-%m-%d %H:%M:%S");
        final <- cbind(final,DateTime);
        
        png(filename = "plot2.png",width = 480,height = 480);
        plot(final$DateTime,final$Global_active_power,type="l",
             xlab = "",
             ylab = "Global Active Power (kilowatts)");
        dev.off();
}