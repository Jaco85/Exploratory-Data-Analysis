downloadfile <- "Data for Peer Assessment.zip"
file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
dir <- "Data for Peer Assessment"

# File download verification. If file does not exist, download to working directory.
if(!file.exists(downloadfile)){
  download.file(file_url,downloadfile, mode = "wb") 
}

# File unzip verification. If the directory does not exist, unzip the downloaded file.
if(!file.exists(dir)){
  unzip("Data for Peer Assessment.zip", files = NULL)
}

#Read/write tabel
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


# Q1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total 
# PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

Y <- NEI$Emissions
X <- NEI$year

q1 <- aggregate(Y ~ X, data = NEI, sum)

Y_Label <- "Emissions totals in ton" 
X_Label <- "Year" 
Main_Label <-"Total emissions by year"

with(q1, plot(X,Y,
              type="b",
              pch=16,
              ylab= Y_Label,
              xlab= X_Label,
              main= Main_Label, 
              xlim = c(1998,2010)))

# Q2: Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510" from 1999 to 2008? Use the base plotting 
# system to make a plot answering this question.

Y <-NEI_BM$Emissions
X <- NEI_BM$year

NEI_BM <- subset(NEI, fips=="24510")
q2 <- aggregate(Y ~ X, data = NEI_BM, sum)

Y_Label <- "Emissions totals in ton" 
X_Label <- "Year" 
Main_Label <-"Total emissions by year in Baltimore City, Maryland"

with(q2, plot(X,Y,
              type="b",
              pch=16,
              ylab= Y_Label,
              xlab= X_Label,
              main= Main_Label, 
              xlim = c(1998,2010)))

# Q3: Of the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable, which of these four sources have seen decreases 
# in emissions from 1999–2008 for Baltimore City? Which have seen increases in 
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


q3 <- aggregate(Emissions~year+type, data=NEI_BM, sum)

qplot(year,Emissions,data=q3,color=type) + 
  stat_smooth(method = "gam") +
  labs(y="PM2.5 Emissions") + 
  labs(title = "Total emissions in the Baltimore City, Maryland by type") 


# Q4: Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?

Sector <- levels(SCC$EI.Sector)
Sector

Coal_emission_codes <- SCC$SCC[grepl("Coal", SCC$EI.Sector)] #select SCC codes were emissions "Coal" is found in sector

Coal_emission <- subset(NEI, SCC %in% Coal_emission_codes) #subset of NEI (Emission codes)

Y <-Coal_emission$Emissions
X <- Coal_emission$year

q4 <- aggregate(Y ~ X, data = Coal_emission, sum)

Y_Label <- "Coal Emissions totals in ton" 
X_Label <- "Year" 
Main_Label <-"Total Coal emissions by year"

with(q4, plot(X,Y,
              type="b",
              pch=16,
              ylab= Y_Label,
              xlab= X_Label,
              main= Main_Label, 
              xlim = c(1998,2010)))

# Q5: How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

Mobile_codes <- SCC$SCC[grepl("Mobile", SCC$EI.Sector)] #select SCC codes were emissions "Coal" is found in sector

Mobile_emission <- subset(NEI, SCC %in% Mobile_codes) #subset of NEI (Emission codes)

Y <-Mobile_emission$Emissions
X <- Mobile_emission$year

q5 <- aggregate(Y ~ X, data = Mobile_emission, sum)

Y_Label <- "Mobile Emissions totals in ton" 
X_Label <- "Year" 
Main_Label <-"Total Mobile emissions by year"

with(q5, plot(X,Y,
              type="b",
              pch=16,
              ylab= Y_Label,
              xlab= X_Label,
              main= Main_Label, 
              xlim = c(1998,2010)))

# Q6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

Mobile_emission_Balt_Cal <- subset(Mobile_emission, fips=="24510" | fips=="06037")

q6 <- aggregate(Emissions~year+fips, data=Mobile_emission_Balt_Cal, sum)

q6$fips[q6$fips == "24510"] <- "Baltimore"
q6$fips[q6$fips == "06037"] <- "Los Angeles"
colnames(q6)[colnames(q6)=="fips"] <- "Country"   

qplot(year,Emissions,data=q6,color=Country) + 
  stat_smooth(method = "gam") +
  labs(y="PM2.5 Emissions") + 
  labs(title = "Baltimore v.s. California Mobile emission") 
