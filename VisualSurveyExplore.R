# Annika Putt
# Created August 28, 2014
# VisualSurveyExplore.r

# load libraries
library(ggplot2)
library(lubridate)

# source the import file to obtain "fishid" data frane
source("VisualSurveyImport.r")
master <- walks

# Create axis labels
axisdates <- format(seq(as.POSIXlt("2010-01-01"), length=12, by="1 month"),"%b-%d")
axisdatesdays <- as.POSIXlt(seq(as.POSIXlt("2010-01-01"), length=12, by="1 month"))$yday

# Create a date range for plotting and subsetting
kodates <- c(212,275)
rbdates <- c(120,180)

##########
### For 2014 I only had counts for one section. In other years I had above and below.
# plot the number of fish seen during the stream walk by location for each species in Carpenter Res

# Remove some of the streams with counts of zero to make the plot a bit cleaner
kosubset <- subset(walks, stream %in% c("truax creek","girl creek","macdonald creek", "sucker creek", "marshall creek"))
# Only plot 2014 for now
kosubset$year <- format(kosubset$date, "%Y")
kosubset$daynumber <- as.POSIXlt(kosubset$date)$yday
kosubset2014 <- subset(kosubset,year=="2014")
kosubset2014 <- subset(kosubset2014,daynumber>kodates[1] & daynumber<kodates[2])
kosubset2015 <- subset(kosubset,year=="2015")
kosubset2015 <- subset(kosubset2015,daynumber>kodates[1] & daynumber<kodates[2])
ko2014  <- ggplot(data=kosubset2014, aes(x=date, y=kocount.above,  group=stream, colour=stream)) + geom_line() + geom_point()
ko2015  <- ggplot(data=kosubset2015, aes(x=date, y=kocount.above,  group=stream, colour=stream)) + geom_line() + geom_point()


# Create a base plot version of the kokanee plot for both years
windows()

plot(subset(kosubset2014,stream=="truax creek")$daynumber,subset(kosubset2014,stream=="truax creek")$kocount,xlab="",ylab="Kokanee Count",
     las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,120),xlim=kodates)
  points(subset(kosubset2014,stream=="girl creek")$daynumber,subset(kosubset2014,stream=="girl creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="red")
  points(subset(kosubset2014,stream=="macdonald creek")$daynumber,subset(kosubset2014,stream=="macdonald creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="darkgrey")
  points(subset(kosubset2014,stream=="sucker creek")$daynumber,subset(kosubset2014,stream=="sucker creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="blue")
  points(subset(kosubset2014,stream=="marshall creek")$daynumber,subset(kosubset2014,stream=="marshall creek")$kocount,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="purple")
  axis(side=1,at=axisdatesdays,labels=axisdates,cex.axis=0.8)
  legend(kodates[1],120,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Sucker Creek","Truax Creek"), pch=c(19,19,19,19,19),bty="n",
         col=c("red","darkgrey","purple","blue","black"))

windows()

plot(subset(kosubset2015,stream=="truax creek")$daynumber,subset(kosubset2015,stream=="truax creek")$kocount,xlab="",ylab="Kokanee Count",
     las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,120),xlim=kodates)
points(subset(kosubset2015,stream=="girl creek")$daynumber,subset(kosubset2015,stream=="girl creek")$kocount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="red")
points(subset(kosubset2015,stream=="macdonald creek")$daynumber,subset(kosubset2015,stream=="macdonald creek")$kocount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="darkgrey")
points(subset(kosubset2015,stream=="sucker creek")$daynumber,subset(kosubset2015,stream=="sucker creek")$kocount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="blue")
points(subset(kosubset2015,stream=="marshall creek")$daynumber,subset(kosubset2015,stream=="marshall creek")$kocount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="purple")
axis(side=1,at=axisdatesdays,labels=axisdates,cex.axis=0.8)
legend(kodates[1],120,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Sucker Creek","Truax Creek"), pch=c(19,19,19,19,19),bty="n",
       col=c("red","darkgrey","purple","blue","black"))


##########
# Do a base plot for rb

# Remove some of the streams with counts of zero to make the plot a bit cleaner
rbsubset <- subset(walks, stream %in% c("truax creek","girl creek","macdonald creek", "marshall creek"))
rbsubset$year <- format(rbsubset$date, "%Y")
rbsubset$daynumber <- as.POSIXlt(rbsubset$date)$yday
rbsubset2014 <- subset(rbsubset,year=="2014")
rbsubset2014 <- subset(rbsubset2014,daynumber>rbdates[1] & daynumber<rbdates[2])
rbsubset2015 <- subset(rbsubset,year=="2015")
rbsubset2015 <- subset(rbsubset2015,daynumber>rbdates[1] & daynumber<rbdates[2])
rb2014  <- ggplot(data=rbsubset2014, aes(x=date, y=rbcount.above,  group=stream, colour=stream)) + geom_line() + geom_point()
rb2015  <- ggplot(data=rbsubset2015, aes(x=date, y=rbcount.above,  group=stream, colour=stream)) + geom_line() + geom_point()


# Create a base plot version of the rainbow plot for both years
windows()

plot(subset(rbsubset2014,stream=="truax creek")$daynumber,subset(rbsubset2014,stream=="truax creek")$rbcount,xlab="",ylab="Rainbow Count",
     las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,13),xlim=rbdates)
points(subset(rbsubset2014,stream=="girl creek")$daynumber,subset(rbsubset2014,stream=="girl creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="red")
points(subset(rbsubset2014,stream=="macdonald creek")$daynumber,subset(rbsubset2014,stream=="macdonald creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="darkgrey")
points(subset(rbsubset2014,stream=="marshall creek")$daynumber,subset(rbsubset2014,stream=="marshall creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="purple")
axis(side=1,at=axisdatesdays,labels=axisdates,cex.axis=0.8)
legend(rbdates[1],13,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Truax Creek"), pch=c(19,19,19,19,19),bty="n",
       col=c("red","darkgrey","purple","black"))

windows()

# This plot sucks becuase there is a bunch of overlap as it is mostly zero
plot(subset(rbsubset2015,stream=="truax creek")$daynumber,subset(rbsubset2015,stream=="truax creek")$rbcount,xlab="",ylab="Rainbow Count",
     las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,13),xlim=rbdates)
points(subset(rbsubset2015,stream=="girl creek")$daynumber,subset(rbsubset2015,stream=="girl creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="red")
points(subset(rbsubset2015,stream=="macdonald creek")$daynumber,subset(rbsubset2015,stream=="macdonald creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="darkgrey")
points(subset(rbsubset2015,stream=="marshall creek")$daynumber,subset(rbsubset2015,stream=="marshall creek")$rbcount,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="purple")
axis(side=1,at=axisdatesdays,labels=axisdates,cex.axis=0.8)
legend(rbdates[1],13,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Truax Creek"), pch=c(19,19,19,19,19),bty="n",
       col=c("red","darkgrey","purple","black"))
#####################
# Plot the sum of all streams for ko and rainbow by date
#####################
                       
# Add day numbers to the file so that plotting can be done for multiple years
datesum$daynumber <- as.POSIXlt(datesum$date)$yday
datesum$year <- format(datesum$date,"%Y")
datesum2014 <- subset(datesum,year=="2014")
datesum2015 <- subset(datesum,year=="2015")

ko2014 <- subset(datesum2014,daynumber>kodates[1] & daynumber<kodates[2])
ko2015 <- subset(datesum2015,daynumber>kodates[1] & daynumber<kodates[2])
rb2014 <- subset(datesum2014,daynumber>rbdates[1] & daynumber<rbdates[2])
rb2015 <- subset(datesum2015,daynumber>rbdates[1] & daynumber<rbdates[2])

windows()
plot(ko2014$daynumber,ko2014$kosums,ylab="Count in all Streams",xlab="",las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,300),xlim=kodates,lwd=2)
points(ko2015$daynumber,ko2015$kosums,ylab="",xlab="",las=1,xaxt="n",pch=19,type="b",lty=2,col="black",lwd=2)
axis(1,at=axisdatesdays,labels=axisdates)
legend(kodates[1],300,lty=c(1,2),lwd=c(2,2),legend=c("2014","2015"),bty="n")

windows()
plot(rb2014$daynumber,rb2014$rbsums,ylab="Count in all Streams",xlab="",las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,16),xlim=rbdates,lwd=2)
points(rb2015$daynumber,rb2015$rbsums,ylab="",xlab="",las=1,xaxt="n",pch=19,type="b",lty=2,col="black",lwd=2)
axis(1,at=axisdatesdays,labels=axisdates)
legend(rbdates[1],16,lty=c(1,2),lwd=c(2,2),legend=c("2014","2015"),bty="n")
