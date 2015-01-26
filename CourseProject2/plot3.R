plot3 <- function() {
	#load in the SCC data
	SCC <- readRDS("summarySCC_PM25.rds");

	#Separate Baltimore data and clean up
	BaltMD <- SCC[SCC$fips == "24510",];
	rm(SCC)

	#Factor the data
	BaltMD$year <- as.factor(BaltMD$year);
	BaltMD$type <- as.factor(BaltMD$type);

	#Calculate the total emissions per year, per type
	#this will give you a weird list. I split the list
	#and reformated it into a data frame.
	split <- split(BaltMD$Emissions,list(BaltMD$year,BaltMD$type));
	sums <- lapply(split,sum);
	names <- names(sums);
	snames <- lapply(names,strsplit,".",fixed=TRUE);
	year <- numeric();
	type <- character();
	data <- numeric();	
	for (i in seq_along(snames)) {year[i] <- snames[[i]][[1]][1]}
	for (i in seq_along(snames)) {type[i] <- snames[[i]][[1]][2]}
	for (i in seq_along(snames)) {data[i] <- sums[[i]]}

	all <- as.data.frame(cbind(year,type,data));

	#This extra step creates a column which rounds the data values
	#so that our y axis doesn't look terrible.
	data2 <- lapply(as.numeric(as.character(all$data)),ceiling);
	data3 <- numeric();
	for (i in seq_along(data2)) {data3[i]<- data2[[i]]}
	all <- cbind(all,data3);
	all$data3 <- as.factor(all$data3);

	#Lets rename that factor, so the y-axis is labeled well
	names(all)[4] <- "Emissions"

	#plot
	png("plot3.png",w=480,h=480)
	library(ggplot2);
	ggplot(all,aes(year, Emissions))+
		geom_smooth(method="lm",se=FALSE,color="black",aes(group=1))+
		geom_point()+
		facet_grid(type~.)
	dev.off();
}