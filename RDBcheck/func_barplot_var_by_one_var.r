barplot_var_by_one_var <- function(x,  Var, var1, tapply_type, sorted = FALSE, graph_par=list(oma = c(1,1,1,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1))
	{
	#
	# prepares a barplot of Var per var1
	# Nuno Prista @ RCM NS&EA 2017
	
	# e.g.: 
		# CA:
			# barplot_var_by_one_var(x = data1$caSub, Var = "Age" , var1 = "FlagCountry", tapply_type = "length")
		# CL: 
			# barplot_var_by_one_var(x = data1$clSub, Var = "OfficialLandingCatchWeight" , var1 = "FlagCountry", tapply_type = "sum")
	windows(10,5); par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)
	if (tapply_type == "length") { t1<-tapply(x[,Var], list(x[,var1]), length); y_title = paste("count of", Var) }
	if (tapply_type == "sum") { t1<-tapply(x[,Var], list(x[,var1]), sum, na.rm=T); y_title = paste("sum of", Var) }
	t1[is.na(t1)]<-0
	if (sorted==TRUE) {t1<-sort(t1, decreasing=T)}
	barplot(t1, las=2, col=rainbow(n = nrow(t1)),ylab = "", main = paste(Var,"by", var1), cex.names = graph_par$cex.x)
	title(ylab=y_title, line = graph_par$ylab_line)
	}
