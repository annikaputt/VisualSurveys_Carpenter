# Annika Putt
# Created September 26, 2014
# VisualSurveyImport.r

# Read in the streamwalk table. 
walks <- read.csv("VisualSurvey_RImport.csv",head=TRUE,
                   colClasses=c("crew"="character","stream"="character","turbidity"="character","comments"="character"))

# List column classes
sapply(walks,class)

# Turn the date into a Date object
# If NAs are appearing, highlight the date column in excel and format as dd-mmm-yy
walks$date <- as.Date(walks$date,format="%d/%b/%y")
