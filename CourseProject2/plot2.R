plot2 <- function(){
	#Load in the SCC data
	SCC <- readRDS("summarySCC_PM25.rds");

	#Separate Baltimore data and clean up
	BaltMD <- SCC[SCC$fips == "24510",];
	BaltMD$year <- as.factor(BaltMD$year);
	rm(SCC)

	#Calculate the total emissions for each
	#year in Baltimore. TotalBalt will be a
	#numeric vector with the years as names.
	TotalBalt <- sapply(split(BaltMD$Emissions,BaltMD$year),sum);

	#open graphics device and plot a simple
	#line graph to confirm whether emissions
	#are decreasing.
	png("plot2.png",w=480,h=480);
	plot(names(TotalBalt),TotalBalt/1000,type="l",
		main = "Emission Trend in Baltimore, MD",
		xlab = "Years",
		ylab = "Emissions in kilotonnes",
		axes=FALSE);
	axis(1,at=names(TotalBalt),label=as.character(names(TotalBalt)));
	axis(2,at=c(0,1.1,2.2,3.3),labels=c(0,1.1,2.2,3.3));
	dev.off();
}