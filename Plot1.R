## Plot 1: Global Active Power

## download and unzip the data file
fileUrl_1 =  "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
f1 = file.path(getwd(), "household_power_consumption.zip")
download.file(fileUrl_1, f1, method = "curl")
unzip("household_power_consumption.zip")

## Load the data set.
df_1 <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "NA")

## change the class of "Date" column form character to data.
df_1$Date <- as.Date(df_1$Date, format = "%d/%m/%Y")

## Subset data from Feb 1 2007 to Feb 2 2007
df_1_subset <- subset(df_1, subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))

## Chage the class of Global_active_power from character to numeric
df_1_subset$Global_active_power <- as.numeric(df_1_subset$Global_active_power)

## Launch a PNG (graphic device) to create a PNG file with a width of 480 pixels and a height of 480 pixels

png(filename = "plot1.png", width = 480, height = 480)
hist(df_1_subset$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
dev.off()