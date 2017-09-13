
library(dplyr)
library(data.table)

yearStart<-2016
yearEnd<-2016
speciesSub<-c("Merlangius merlangus")
areaSub<-c("27.4","27.7.d")
stock="whg.27.47d"
#StockSub<- for the future

cl<-read.csv("H:/RCM/CL 2009-2016 NSEA.csv", header=T, stringsAsFactors=FALSE)
cl$LandingCountry<-cl$ï..LandingCountry
ce<-read.csv("H:/RCM/CE 2009-2016 NSEA.csv", header=T, sep="," , stringsAsFactors=FALSE)
ce$FlagCountry<-ce$ï..FlagCountry
tr<-read.csv("H:/RCM/TR NSEA.csv", header=T, sep=",", stringsAsFactors=FALSE)
tr$CS_TripId<-tr$ï..CS_TripId
hh<-read.csv("H:/RCM/HH NSEA.csv", header=T, sep=",", stringsAsFactors=FALSE)
hh$CS_StationId<-hh$ï..CS_StationId
hh$CS_TripId<-as.character(hh$CS_TripId)
sl<-read.csv("H:/RCM/SL NSEA.csv", header=T, sep=",", stringsAsFactors=FALSE)
sl$CS_SpeciesListId<-sl$ï..CS_SpeciesListId
sl$CS_StationId<-as.character(sl$CS_StationId)
hl<-read.csv("H:/RCM/HL NSEA.csv", header=T, sep=",", stringsAsFactors=FALSE)
hl$CS_LengthId<-hl$ï..CS_LengthId
hl$CS_SpeciesListId<-as.character(hl$CS_SpeciesListId)
ca<-read.csv("H:/RCM/CA NSEA.csv", header=T, sep=",", stringsAsFactors=FALSE)
ca$CS_SMAWLId<-ca$ï..CS_SMAWLId

#Subset cl on year, area and species
clSub<-subset(cl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSub & Area %like% areaSub)

#Subset ce on year and area
ceSub<-subset(ce, Year>=yearStart & Year<=yearEnd & Area %like% areaSub)

#Subset the tr record on year and area
trhh<-left_join(tr,hh)
trhhSub<-subset(trhh, Year>=yearStart & Year<=yearEnd & Area %like% areaSub)
trSub<-distinct(trhhSub[,c(names(tr))], CS_TripId)

#Subset the hh record on year and area
hhSub<-subset(hh, Year>=yearStart & Year<=yearEnd & Area %like% areaSub)

#Subset the sl record on year, area and species
hhsl<-left_join(hh,sl)
hhslSub<-subset(hhsl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSub & Area %like% areaSub)
slSub<-hhslSub[,c(names(sl))]

#Subset the hl record on year, area and species
hhslhl<-left_join(hhsl,hl)
hhslhlSub<-subset(hhslhl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSub & Area %like% areaSub)
hlSub<-hhslhlSub[,c(names(hl))]

#Subset ca on year, area and species
caSub<-subset(ca, Year>=yearStart & Year<=yearEnd & Species %in% speciesSub & Area %like% areaSub)

#Creating a universal country list for that stock -> similar colors in all graphs
ctrs_list = sort(unique(c(trSub$FlagCountry,trSub$LandingCountry, clSub$FlagCountry,clSub$LandingCountry, ceSub$FlagCountry)))

