

library(dplyr)
library(data.table)

#yearStart<-2016
#yearEnd<-2016
#speciesSelection<-c("Merlangius merlangus")
#areaSelection<-c("27.4","27.7.d")
#stock="whg.27.47d"
#StockSub<- for the future
#dir_data<-"H:/RCM/"

#data1<-read_in_subset_rdbData(dir_data="H:/RCM/", yearStart=2016, yearEnd=2016, speciesSelection=c("Merlangius merlangus"), 
                       areaSelection=c("27.4","27.7.d"), stock="whg.27.47d")

read_in_subset_rdbData<-function(dir_data, yearStart, yearEnd, speciesSelection, areaSelection, stock)
{
  
stock<-stock

cl<-read.csv(paste(dir_data, "CL 2009-2016 NSEA.csv", sep = ""), header=T, stringsAsFactors=FALSE)
cl$LandingCountry<-cl$ï..LandingCountry
ce<-read.csv(paste(dir_data, "CE 2009-2016 NSEA.csv", sep =""), header=T, sep="," , stringsAsFactors=FALSE)
ce$FlagCountry<-ce$ï..FlagCountry
tr<-read.csv(paste(dir_data, "TR NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
tr$CS_TripId<-tr$ï..CS_TripId
hh<-read.csv(paste(dir_data, "HH NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
hh$CS_StationId<-hh$ï..CS_StationId
hh$CS_TripId<-as.character(hh$CS_TripId)
sl<-read.csv(paste(dir_data, "SL NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
sl$CS_SpeciesListId<-sl$ï..CS_SpeciesListId
sl$CS_StationId<-as.character(sl$CS_StationId)
hl<-read.csv(paste(dir_data, "HL NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
hl$CS_LengthId<-hl$ï..CS_LengthId
hl$CS_SpeciesListId<-as.character(hl$CS_SpeciesListId)
ca<-read.csv(paste(dir_data, "CA NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
ca$CS_SMAWLId<-ca$ï..CS_SMAWLId


#Subset cl on year, area and species
clSub<-subset(cl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))

#Subset ce on year and area
ceSub<-subset(ce, Year>=yearStart & Year<=yearEnd & Area %like% paste(areaSelection, collapse='|'))

#Subset the tr record on year and area
trhh<-left_join(tr,hh)
trhhSub<-subset(trhh, Year>=yearStart & Year<=yearEnd & Area %like% paste(areaSelection, collapse='|'))
trSub<-distinct(trhhSub[,c(names(tr))], CS_TripId)

#Subset the hh record on year and area
hhSub<-subset(hh, Year>=yearStart & Year<=yearEnd & Area %like% paste(areaSelection, collapse='|'))

#Subset the sl record on year, area and species
hhsl<-left_join(hh,sl)
hhslSub<-subset(hhsl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))
slSub<-hhslSub[,c(names(sl))]

#Subset the hl record on year, area and species
hhslhl<-left_join(hhsl,hl)
hhslhlSub<-subset(hhslhl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))
hlSub<-hhslhlSub[,c(names(hl))]

#Subset ca on year, area and species
caSub<-subset(ca, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))

#Creating a universal country list for that stock -> similar colors in all graphs
ctrs_list = sort(unique(c(trSub$FlagCountry,trSub$LandingCountry, clSub$FlagCountry,clSub$LandingCountry, ceSub$FlagCountry)))

x<-(list(stock,cl,ce,tr,hh,sl,hl,ca,clSub,ceSub,trSub,hhSub,slSub,hlSub,caSub,ctrs_list))

return(x)

}