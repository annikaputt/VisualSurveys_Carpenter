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
### For 2014 I only had counts for one section. In other years I had above and below.
# plot the number of fish seen during the stream walk by location for each species in Carpenter Res

# Remove some of the streams with counts of zero to make the plot a bit cleaner
kosubset <- subset(walks, stream %in% c("truax creek","girl creek","macdonald creek", "sucker creek", "marshall creek"))
# Only plot 2014 for now
kosubset$year <- format(kosubset$date, "%Y")
kosubset <- subset(kosubset,year=="2014")
kosubset2 <- subset(kosubset,date > "2014-07-01")
ko  <- ggplot(data=kosubset2, aes(x=date, y=kocount.above,  group=stream, colour=stream)) + geom_line() + geom_point()

# Create a base plot version of the kokanee plot
windows()

Months        <- as.Date(as.vector(as.character(seq.Date(
  as.Date(strptime(140701,"%y%m%d")),
  as.Date(strptime(141030,"%y%m%d")),
  by="week"))),format="%Y-%m-%d")
Months        <- Months[seq(1,length(Months),by=1)]
labels <- format(Months, "%d-%b-%y")

kosubset2 <- subset(kosubset,date > "2014-07-01" & date < "2014-09-30")

plot(subset(kosubset2,stream=="truax creek")$date,subset(kosubset2,stream=="truax creek")$kocount.above,xlab="",ylab="Kokanee Count",
     las=1,xaxt="n",pch=19,type="b",col="black",ylim=c(0,120))
  points(subset(kosubset2,stream=="girl creek")$date,subset(kosubset2,stream=="girl creek")$kocount.above,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="red")
  points(subset(kosubset2,stream=="macdonald creek")$date,subset(kosubset2,stream=="macdonald creek")$kocount.above,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="darkgrey")
  points(subset(kosubset2,stream=="sucker creek")$date,subset(kosubset2,stream=="sucker creek")$kocount.above,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="blue")
  points(subset(kosubset2,stream=="marshall creek")$date,subset(kosubset2,stream=="marshall creek")$kocount.above,xlab="",ylab="",
        las=1,xaxt="n",pch=19,type="b",col="purple")
  axis(side=1,at=Months,labels=labels,cex.axis=0.8)
  legend(max(Months)-48,120,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Sucker Creek","Truax Creek"), pch=c(19,19,19,19,19),bty="n",
         col=c("red","darkgrey","purple","blue","black"))

##########
# Do a base plot for rb

# Only plot 2014 for now
rbsubset <- subset(walks,date > "2014-03-01" & date < "2014-08-19")
rbsubset$year <- format(rbsubset$date, "%Y")
rbsubset <- subset(rbsubset, stream %in% c("truax creek","girl creek","macdonald creek", "marshall creek"))
rbsubset <- rbsubset[order(rbsubset$date, - as.numeric(rbsubset$date)), ] 

rb  <- ggplot(data=rbsubset, aes(x=date, y=rbcount.above,  group=stream, colour=stream)) + geom_line() + geom_point()


windows()

MonthsRB    <- as.Date(as.vector(as.character(seq.Date(
  as.Date(strptime(140301,"%y%m%d")),
  as.Date(strptime(140820,"%y%m%d")),
  by="week"))),format="%Y-%m-%d")
MonthsRB    <- MonthsRB[seq(1,length(MonthsRB),by=1)]
labelsRB <- format(MonthsRB, "%d-%b-%y")



plot(subset(rbsubset,stream=="marshall creek")$date,subset(rbsubset,stream=="marshall creek")$rbcount.above,xlab="",ylab="Rainbow Count",
     las=1,xaxt="n",pch=19,type="b",col="purple",ylim=c(0,12))
points(subset(rbsubset,stream=="girl creek")$date,subset(rbsubset,stream=="girl creek")$rbcount.above,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="red")
points(subset(rbsubset,stream=="macdonald creek")$date,subset(rbsubset,stream=="macdonald creek")$rbcount.above,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="darkgrey")
points(subset(rbsubset,stream=="truax creek")$date,subset(rbsubset,stream=="truax creek")$rbcount.above,xlab="",ylab="",
       las=1,xaxt="n",pch=19,type="b",col="black")
axis(side=1,at=MonthsRB,labels=labelsRB,cex.axis=0.8)
legend(max(MonthsRB)-75,12,legend=c("Girl Creek","Macdonald Creek","Marshall Creek","Truax Creek"), pch=c(19,19,19,19),bty="n",
       col=c("red","darkgrey","purple","black"))

