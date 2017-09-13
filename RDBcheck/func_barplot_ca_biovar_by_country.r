#This looks cool

barplot_ca_biovar_by_country <- function(x, SamplingType, CatchCategory, BioVar, by.x, countries_list)
	{
	# prepares a barplot of bio_var per country
	# RCM NS&EA 2017
	
	# subset of target data
	y<-droplevels(x[x$SamplingType == SamplingType & x$CatchCategory == CatchCategory & x[, BioVar]!="NULL",])
	y[, BioVar]<-as.numeric(as.character(y[, BioVar]))
	t1<-tapply(y[,BioVar], factor(as.character(y[,by.x]), levels=countries_list), length)
	t1[is.na(t1)]<-0
	ylimit = c(0, max(t1)*1.1)
	barplot(t1, names.arg = countries_list, ylim= ylimit, las=2, col="gray", main = paste(BioVar,"by", by.x))
	}
# barplot_ca_biovar_by_country(x = ca_data16[ca_data16$Species=="Gadus morhua",], SamplingType = "M", CatchCategory = "LAN", BioVar = "Age", by.x="FlagCountry", countries_list = ctrs_list)	
