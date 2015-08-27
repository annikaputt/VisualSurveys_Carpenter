# Annika Putt
# Created August 28, 2014
# VisualSurveyExplore.r

# load libraries
library(ggplot2)
library(lubridate)

# source the import file to obtain "fishid" data frane
source("VisualSurveyImport.r")
master <- walks
##########
# plot the number of fish seen during the stream walk by location for each species in Carpenter Res

# Remove some of the streams with counts of zero to make the plot a bit cleaner
kosubset <- subset(walks, stream %in% c("truax creek","girl creek","macdonald creek", "sucker creek", "marshall creek"))
# Only plot 2014 for now
kosubset$year <- format(kosubset$date, "%Y")
kosubset <- subset(kosubset,year=="2014")
kosubset2 <- subset(kosubset,date > "2014-07-01")
ko  <- ggplot(data=kosubset2, aes(x=date, y=kocount,  group=stream, colour=stream)) + geom_line() + geom_point()

# Code for other species plots. Counts are so low that these aren't really useful right now.
bsu <- ggplot(data=walks, aes(x=date, y=bsucount, group=stream, colour=stream)) + geom_line() + geom_point()
rb  <- ggplot(data=walks, aes(x=date, y=rbcount,  group=stream, colour=stream)) + geom_line() + geom_point()
bt  <- ggplot(data=walks, aes(x=date, y=btcount,  group=stream, colour=stream)) + geom_line() + geom_point()

# Create a base plot version of the kokanee plot
windows()

Months        <- as.Date(as.vector(as.character(seq.Date(
  as.Date(strptime(140701,"%y%m%d")),
  as.Date(strptime(141030,"%y%m%d")),
  by="week"))),format="%Y-%m-%d")
Months        <- Months[seq(1,length(Months),by=1)]
labels <- format(Months, "%d-%b-%y")

kosubset2 <- subset(kosubset,date > "2014-07-01")

plot(subset(kosubset2,stream=="truax creek")$date,subset(kosubset2,stream=="truax creek")$kocount,xlab="",ylab="Kokanee Count",
     las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,120))
  points(subset(kosubset2,stream=="girl creek")$date,subset(kosubset2,stream=="girl creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="red")
  points(subset(kosubset2,stream=="macdonald creek")$date,subset(kosubset2,stream=="macdonald creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="darkgrey")
  points(subset(kosubset2,stream=="sucker creek")$date,subset(kosubset2,stream=="sucker creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="blue")
  points(subset(kosubset2,stream=="marshall creek")$date,subset(kosubset2,stream=="marshall creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="purple")
  axis(side=1,at=Months,labels=labels,cex.axis=0.8)
  legend(max(Months)-39,120,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Sucker Creek","Truax Creek"), pch=c(19,19,19,19,19),bty="n",
         col=c("red","darkgrey","purple","blue","black"))

##########
# Do a base plot for rb
windows()

MonthsRB    <- as.Date(as.vector(as.character(seq.Date(
  as.Date(strptime(140301,"%y%m%d")),
  as.Date(strptime(140820,"%y%m%d")),
  by="week"))),format="%Y-%m-%d")
MonthsRB    <- MonthsRB[seq(1,length(MonthsRB),by=1)]
labelsRB <- format(MonthsRB, "%d-%b-%y")

rbsubset <- subset(walks,date > "2014-03-01" & date < "2014-08-20")
rbsubset$year <- format(rbsubset$date, "%Y")
rbsubset <- subset(rbsubset, stream %in% c("truax creek","girl creek","macdonald creek", "marshall creek"))
rbsubset <- rbsubset[order(rbsubset$date, - as.numeric(rbsubset$date)), ] 

plot(subset(rbsubset,stream=="marshall creek")$date,subset(rbsubset,stream=="marshall creek")$rbcount,xlab="",ylab="Rainbow Count",
     las=1,xaxt="n",pch=19,type="b",col="purple",ylim=c(0,12))
points(subset(rbsubset,stream=="girl creek")$date,subset(rbsubset,stream=="girl creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="red")
points(subset(rbsubset,stream=="macdonald creek")$date,subset(rbsubset,stream=="macdonald creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="darkgrey")
points(subset(rbsubset,stream=="truax creek")$date,subset(rbsubset,stream=="truax creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="black")
axis(side=1,at=MonthsRB,labels=labelsRB,cex.axis=0.8)
legend(max(MonthsRB)-20,12,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Truax Creek"), pch=c(19,19,19,19),bty="n",
       col=c("red","darkgrey","purple","black"))

##########
# Create a plot just with the timing of appearance for the 4 species in any creek (i.e., was observed somewhere in the watershed)
walks$kopres  <- NA
walks$rbpres  <- NA
walks$btpres  <- NA
walks$bsupres <- NA


for (i in 1:nrow(walks)) {
  if (walks$kocount[i] > 0 | is.na(walks$kocount[i]))  walks$kopres[i]  <- 1 # At this time NA in the ko counts means they were seen at the hrb 
  # bridge but no count was obtained due to turbidity.
  if (walks$btcount[i] > 0)  walks$btpres[i]  <- 1
  if (walks$bsucount[i] > 0) walks$bsupres[i] <- 1
  if (walks$rbcount[i] > 0)  walks$rbpres[i]  <- 1
}

# need to make a new table with one row for each date
presence.summary <- aggregate(walks[c("kopres","rbpres","bsupres","btpres")], by=list(date=walks$date), FUN=sum, na.rm=TRUE) 

# change the numbers in the presence.summary to make a simple line plot
for (i in 1:nrow(presence.summary)) {
  if (presence.summary$kopres[i] > 0)  presence.summary$kopres[i]  <- 4 
  if (presence.summary$btpres[i] > 0)  presence.summary$btpres[i]  <- 3
  if (presence.summary$bsupres[i] > 0) presence.summary$bsupres[i] <- 2
  if (presence.summary$rbpres[i] > 0)  presence.summary$rbpres[i]  <- 1
}

# plot the appearance of fish in the watershed
# turn zeros into na 
# presence.summary <- replace(presence.summary, presence.summary==0, "NA") # commented out because it results in graphical warnings

# Create a function to make a date seq that starts on the first day of the first month of observed fish
DateSeq <- function(WalkDates) {
  firstDate       <- min(WalkDates)
  firstDate.month <- month(firstDate)
  firstDate.year  <- year(firstDate)
  firstDate.day   <- 1
  start           <- as.Date(ymd(paste(firstDate.year, firstDate.month, firstDate.day, sep = "-")))
  Sequance        <- seq(start,max(presence.summary$date),"month")
}

date.seq <- DateSeq(presence.summary$date)

windows()
par(mar=c(7,9,3,3))
plot(presence.summary$date,presence.summary$kopres,pch=15,ylim=c(0.5,4.5),xlim=c(min(presence.summary$date),
     max(presence.summary$date)),yaxt="n",xaxt="n",ylab=NA,xlab=NA)
 points(presence.summary$date,presence.summary$rbpres,pch=15)
 points(presence.summary$date,presence.summary$btpres,pch=15)
 points(presence.summary$date,presence.summary$bsupres,pch=15)
 axis(side=2,at=c(1,2,3,4),labels=c("Rainbow Trout","Bridgelip Sucker","Bull Trout","Kokanee"),las=1)
 axis(side=1,at=date.seq,labels=format(date.seq,"%b/%Y"),las=3)
 abline(h=1:4,lty=6,col="lightgrey")
 abline(v=date.seq,lty=6,col="lightgrey")

##########
# Create a similar graphs but just for Kokanee, and this time separated by stream
walksko <- kosubset
walksko$stream <- as.factor(walksko$stream)

for (i in 1:nrow(walksko)) {
  if (walksko$kocount[i] > 0 & walksko$stream[i] == "girl creek")  walksko$kopres[i]  <- 5 
  if (walksko$kocount[i] > 0 & walksko$stream[i] == "macdonald creek")  walksko$kopres[i]  <- 4
  if (walksko$kocount[i] > 0 & walksko$stream[i] == "marshall creek")  walksko$kopres[i]  <- 3
  if (walksko$kocount[i] > 0 & walksko$stream[i] == "sucker creek")  walksko$kopres[i]  <- 2
  if (walksko$kocount[i] > 0 & walksko$stream[i] == "truax creek")  walksko$kopres[i]  <- 1 
  if (walksko$kocount[i] == 0)  walksko$kopres[i]  <- 0 
}

walksko <- subset(walksko,kopres>0)

# Create a new date seqence
ko.date <- seq(min(walksko$date)-15,max(walksko$date)+15,"week")

windows()
par(mar=c(7,9,3,3))
plot(subset(walksko,stream=="girl creek")$date,subset(walksko,stream=="girl creek")$kopres,type="l",ylim=c(0.5,5.5),xlim=c(min(walksko$date)-15,
     max(walksko$date)+15),yaxt="n",xaxt="n",ylab=NA,xlab=NA,lwd=9)
points(subset(walksko,stream=="macdonald creek")$date,subset(walksko,stream=="macdonald creek")$kopres,type="l",lwd=9)
points(subset(walksko,stream=="marshall creek")$date,subset(walksko,stream=="marshall creek")$kopres,type="l",lwd=9)
points(subset(walksko,stream=="sucker creek")$date,subset(walksko,stream=="sucker creek")$kopres,type="l",lwd=9)
points(subset(walksko,stream=="truax creek")$date,subset(walksko,stream=="truax creek")$kopres,type="l",lwd=9)
axis(side=2,at=c(1,2,3,4,5),labels=c("Truax Creek","Sucker Creek","Marshall Creek","Macdonald Creek","Girl Creek"),las=1)
axis(side=1,at=ko.date,labels=format(ko.date,"%d-%b-%Y"),las=3)

