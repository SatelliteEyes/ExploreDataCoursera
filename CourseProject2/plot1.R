plot1 <- function (){
	#Load in the SCC data
	SCC <- readRDS("summarySCC_PM25.rds");
	
	#Calculate the total Emissions for each year. Total
	#will be a numeric vector with the years as names.
	Total <- sapply(split(SCC$Emissions,SCC$year),sum);

	#open the graphic device and plot a simple line graph
	#to confirm whether emissions are decreasing. For the
	#sake of prettiness, we're adding lables and cleaning
	#up the axes, too.
	png("plot1.png",w=480,h=480);
	plot(names(Total),Total/1000,type="l",
		main = "Emission Trend",
		xlab = "Years", 
		ylab = "Emissions in kilotonnes",
		axes=FALSE);
	axis(1,at=names(Total),label=as.character(names(Total)));
	axis(2,at=c(0,1480,2960,4440,5920,7400),label=c(0,1480,2960,4440,5920,7400));
	dev.off();
}