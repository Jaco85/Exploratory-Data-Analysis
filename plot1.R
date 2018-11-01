downloadfile <- "household_power_consumption.zip"
file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
dir <- "Household_power_consumption"

# File download verification. If file does not exist, download to working directory.
if(!file.exists(downloadfile)){
  download.file(file_url,downloadfile, mode = "wb") 
}

# File unzip verification. If the directory does not exist, unzip the downloaded file.
if(!file.exists(dir)){
  unzip("household_power_consumption.zip", files = NULL, dir=".")
}

#Read/write tabel, where "?" is a missing value
HPS <- data.table::fread(input = "household_power_consumption.txt"
                             , na.strings="?"
)

#Convert character in "Date" to date variable
HPS[, Date := lapply(.SD, as.Date, "%d/%m/%Y"), .SDcols = c("Date")]

#Filter: only given dates
HPS <- HPS[(Date >= "2007-02-01") & (Date <= "2007-02-02")]

#Create and save PNG plot

png(file="plot1.png", width=480, height=480)
with(HPS,hist(Global_active_power,col="red",main="Global Active Power",
                    xlab="Global Active Power (kilowatts)", ylab="Frequency"))
dev.off()
