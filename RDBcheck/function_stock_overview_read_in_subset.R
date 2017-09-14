

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
                       #areaSelection=c("27.4","27.7.d"), stock="whg.27.47d")

read_in_subset_rdbData<-function(dir_data, yearStart, yearEnd, speciesSelection, areaSelection, stock)
{
print("This function requeries the following packages "dplyr", "data.table"")
  
stock<-stock
print("reading cl...")
cl<-read.csv(paste(dir_data, "CL 2009-2016 NSEA.csv", sep = ""), header=T, stringsAsFactors=FALSE)
cl$LandingCountry<-cl$ï..LandingCountry
print("reading ce...")
ce<-read.csv(paste(dir_data, "CE 2009-2016 NSEA.csv", sep =""), header=T, sep="," , stringsAsFactors=FALSE)
ce$FlagCountry<-ce$ï..FlagCountry
print("reading cs-tr...")
tr<-read.csv(paste(dir_data, "TR NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
tr$CS_TripId<-tr$ï..CS_TripId
print("reading cs-hh...")
hh<-read.csv(paste(dir_data, "HH NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
hh$CS_StationId<-hh$ï..CS_StationId
hh$CS_TripId<-as.character(hh$CS_TripId)
print("reading cs-sl...")
sl<-read.csv(paste(dir_data, "SL NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
sl$CS_SpeciesListId<-sl$ï..CS_SpeciesListId
sl$CS_StationId<-as.character(sl$CS_StationId)
print("reading cs-hl...")
hl<-read.csv(paste(dir_data, "HL NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
hl$CS_LengthId<-hl$ï..CS_LengthId
hl$CS_SpeciesListId<-as.character(hl$CS_SpeciesListId)
print("reading cs-ca...")
ca<-read.csv(paste(dir_data, "CA NSEA.csv", sep =""), header=T, sep=",", stringsAsFactors=FALSE)
ca$CS_SMAWLId<-ca$ï..CS_SMAWLId


#Subset cl on year, area and species
print("Subset cl on year, area and species...")
clSub<-subset(cl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))

#Subset ce on year and area
print("Subset ce on year and area...")
ceSub<-subset(ce, Year>=yearStart & Year<=yearEnd & Area %like% paste(areaSelection, collapse='|'))

#Subset the tr record on year and area
print("Subset cs-tr on year and area...")
trhh<-left_join(tr,hh)
trhhSub<-subset(trhh, Year>=yearStart & Year<=yearEnd & Area %like% paste(areaSelection, collapse='|'))
trSub<-distinct(trhhSub[,c(names(tr))], CS_TripId)

#Subset the hh record on year and area
print("Subset cs-hh on year and area...")
hhSub<-subset(hh, Year>=yearStart & Year<=yearEnd & Area %like% paste(areaSelection, collapse='|'))

#Subset the sl record on year, area and species
print("Subset cs-sl on year, area and species...")
hhsl<-left_join(hh,sl)
hhslSub<-subset(hhsl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))
slSub<-hhslSub[,c(names(sl))]

#Subset the hl record on year, area and species
print("Subset cs-hl on year, area and species...")
hhslhl<-left_join(hhsl,hl)
hhslhlSub<-subset(hhslhl, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))
hlSub<-hhslhlSub[,c(names(hl))]

#Subset ca on year, area and species
print("Subset cs-ca on year, area and species...")
caSub<-subset(ca, Year>=yearStart & Year<=yearEnd & Species %in% speciesSelection & Area %like% paste(areaSelection, collapse='|'))

#Creating a universal country list for that stock -> similar colors in all graphs
print("Creating a universal country list for that stock...")
ctrs_list = sort(unique(c(trSub$FlagCountry,trSub$LandingCountry, clSub$FlagCountry,clSub$LandingCountry, ceSub$FlagCountry)))

# Setting factors for FlagCountry and LandingCountry
print("Setting factors for FlagCountry and LandingCountry...") 
for (i in c("FlagCountry","LandingCountry"))
    {
   if (i %in% colnames(clSub)) {clSub[,i]<-factor(clSub[,i], levels = ctrs_list)}
   if (i %in% colnames(ceSub)) {ceSub[,i]<-factor(ceSub[,i], levels = ctrs_list)}
   if (i %in% colnames(trSub)) {trSub[,i]<-factor(trSub[,i], levels = ctrs_list)}
   if (i %in% colnames(hhSub)) {hhSub[,i]<-factor(hhSub[,i], levels = ctrs_list)}
   if (i %in% colnames(slSub)) {slSub[,i]<-factor(slSub[,i], levels = ctrs_list)}
   if (i %in% colnames(hlSub)) {hlSub[,i]<-factor(hlSub[,i], levels = ctrs_list)}
   if (i %in% colnames(caSub)) {caSub[,i]<-factor(caSub[,i], levels = ctrs_list)}    
    }
  
x<-list(stock = stock, cl = cl, ce = ce, tr = tr, hh = hh, sl = sl, hl = hl, ca = ca, clSub = clSub, ceSub = ceSub, trSub = trSub, hhSub = hhSub, slSub = slSub, hlSub = hlSub, caSub = caSub, ctrs_list = ctrs_list)

return(x)

}
