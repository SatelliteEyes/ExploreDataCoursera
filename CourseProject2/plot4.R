plot4 <- function() {
	#load in the SCC data and NEI data
	SCC <- readRDS("summarySCC_PM25.rds");
	NEI<- readRDS("Source_Classification_Code.rds");

	#Merge data so the the human readable descriptions
	#are in the same data frame as the missions data
	library(plyr);
	Full <- join(SCC,NEI,by="SCC");
	rm(list=c("SCC","NEI"));

	#Find the Coal related rows.
	Coalind <- grep(' [Cc]oal ?',Full)
	Coal <- Full[Coalind,]
	rm(Full)

	#Find the Combustion related rows
	Combind <- grep(' [Cc]omb',Coal)
	CoalComb <- Coal[Combind,]
	rm(Coal)
	
	#Factor the data
	CoalCombyear <- as.factor(CoalComb$year);

	#Calculate the total emissions per year
	#this will give you a weird list. I split the list
	#and reformated it into a data frame.
	Total <- sapply(split(CoalComb$Emissions,CoalComb$year),sum);

	png("plot4.png",w=480,h=480)
	xyplot(Emissions ~ year,CoalComb,main="Trend in Coal Related Emissions",
		xlab="Year",
		ylab="Emissions")
	dev.off()
}