# Script for creating Plot #2

# Clean workspace
rm(list = ls())

# Load packages
library(lubridate)


# Following chunk contains the same code exactly as plot1.R
# Load data ===================================
#
# Check if raw data is on the working directory
if(!("household_power_consumption.txt" %in% list.files())) {
        stop("Required dataset not avaliable on working directory \n Stopping execution")
}

# Each row of the dataset represents observations for a determined Minute in time
# We are only interested on the data for 2007/02/01 and 2007/02/02
# Since the dataset is ordered by ascending date and time, in order to load only desired dates
# we can count the amount of minutes between the first observation up to the desired one
# This will be the number of rows we can skip
# We will read only rows corresponding to the number of minutes in those 2 days
#
# Auxiliary function to calculate minutes between dates
# Makes use of functions from lubridate package
min_between_dates <- function(day_from, day_to, min_from = "00:00:00", min_to = "00:00:00" ) {
        day_start <- dmy_hms(paste0(day_from, min_from))
        day_end <- dmy_hms(paste0(day_to, min_to))
        abs(difftime(day_start, day_end, units = "min"))
}

# Read first line of the dataset to register starting date
ds_start <- read.table("household_power_consumption.txt", header = TRUE,
                       sep = ";", nrow = 1, colClasses = "character")

# Calculate rows to skip and read
rows_skip <- min_between_dates(ds_start$Date[1], "01/02/2007", ds_start$Time[1]) + 1
rows_read <- min_between_dates("01/02/2007", "03/02/2007")

# Read dataset only with desired rows
ds <- read.table("household_power_consumption.txt", header = FALSE,
                 sep = ";", col.names = names(ds_start), colClasses = "character",
                 skip = rows_skip, nrow = rows_read)

# Combine date and time and typecast to POSIX with lubridate
# Add date_time to dataset
# Typecast remaining collumns to numeric
ds[, 3:9] <- lapply(ds[, 3:9], as.numeric)
datetime <- dmy_hms(paste0(ds$Date, " ", ds$Time))
ds <- cbind(ds, datetime)

# Create Plot 2 =======================================
#
# Open PNG device
# Using transparent background to replicate plots on the repo
png("plot2.png", bg = "transparent")

# Write Plot to device
# Start with empty plot
# Use lines function to connect the points
plot(ds$Global_active_power ~ ds$datetime, 
     type = "n",
     xlab = "",
     ylab = "Global Actibe Power (kilowatts)"
)
lines(ds$Global_active_power ~ ds$datetime)

# Close Device
dev.off()