# Annika Putt
# Created September 26, 2014
# VisualSurveyImport.r

library(plyr)

# Read in the streamwalk table. 
walks <- read.csv("VisualSurvey_RImport.csv",head=TRUE,
                   colClasses=c("crew"="character","stream"="character","viz"="character","discharge"="character","comments"="character"))

# List column classes
sapply(walks,class)

# Turn the date into a Date object
# If NAs are appearing, highlight the date column in excel and format as dd-mmm-yy
walks$date <- as.POSIXct(walks$date,format="%d/%b/%y")

# Create a single count column (sum above and below full pool) for rainbow and kokanee
walks$kocount <- rowSums(walks[9:10],na.rm=TRUE)
walks$rbcount <- rowSums(walks[11:12],na.rm=TRUE)

# Create a table that sums all counts for each date at a select stream set
walkstosum <- subset(walks, stream %in% c("truax creek","girl creek","macdonald creek", "sucker creek", "marshall creek"))
datesum <- ddply(walkstosum, c("date"), summarise,
               kosums = sum(kocount,na.rm=TRUE),
               rbsums = sum(rbcount,na.rm=TRUE))
datesum <- subset(datesum,date > "2013-09-06")
