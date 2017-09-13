

library(data.table)

yearStart<-2016
yearEnd<-2016
speciesSub<-c("Merlangius merlangus")
areaSub<-c("27.4","27.7.d")
stock="whg.27.47d"
#StockSub<- for the future

cl<-read.csv("H:/RCM/CL 2009-2016 NSEA.csv", head=T, sep="," , stringsAsFactors=FALSE)
ce<-read.csv("H:/RCM/CE 2009-2016 NSEA.csv", head=T, sep="," , stringsAsFactors=FALSE)
tr<-read.csv("H:/RCM/TR NSEA.csv", head=T, sep=",", stringsAsFactors=FALSE)
hh<-read.csv("H:/RCM/HH NSEA.csv", head=T, sep=",", stringsAsFactors=FALSE)
sl<-read.csv("H:/RCM/SL NSEA.csv", head=T, sep=",", stringsAsFactors=FALSE)
hl<-read.csv("H:/RCM/HL NSEA.csv", head=T, sep=",", stringsAsFactors=FALSE)
ca<-read.csv("H:/RCM/CA NSEA.csv", head=T, sep=",", stringsAsFactors=FALSE)

clSub<-subset(cl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSub & Area %like% areaSub)
ceSub<-subset(ce, Year>=yearStart & Year<=yearEnd & Area %like% areaSub)
trSub<-subset(tr, Year>=yearStart & Year<=yearEnd)
hhSub<-subset(hh, Year>=yearStart & Year<=yearEnd & Area %like% areaSub)

hhsl<-left_join

caSub<-subset(ca, Year>=yearStart & Year<=yearEnd & Species %in% speciesSub & Area %like% areaSub)

#Creating a universal country list for that stock -> similar colors in all graphs
ctrs_list = sort(unique(c(trSub$FlagCountry,trSub$LandingCountry, clSub$FlagCountry,clSub$LandingCountry, ceSub$FlagCountry)))
