barplot_var_by_two_var_stacked <- function(x,  Var, var1, var2, tapply_type, sorted = FALSE, graph_par=list(oma = c(1,1,1,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1))
	{
	# Prepares a barplot of Var per var1 (with var 2 stacked)
	# Nuno Prista @ RCM NS&EA 2017
	
	# e.g.: 
		# CA:
			# barplot_var_by_two_var_stacked(x = data1$caSub,  Var = "Age", var1 = "FlagCountry", var2 = "CatchCategory", tapply_type = "length")
		# CL: 
			# barplot_var_by_two_var_stacked(x = data1$clSub,  Var = "OfficialLandingCatchWeight", var1 = "FlagCountry", var2 = "Area", tapply_type = "sum")
			
	windows(10,5); par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)
	if (tapply_type == "length") { t1<-tapply(x[,Var], list(x[,var2],x[,var1]), length); y_title = paste("count of", Var) }
	if (tapply_type == "sum") { t1<-tapply(x[,Var], list(x[,var2],x[,var1]), sum, na.rm=T);  y_title = paste("sum of", Var) }
	t1[is.na(t1)]<-0
	if(sorted==TRUE) {v1<-names(sort(apply(t1,2,sum), decreasing=T)); t1<-t1[,v1]}
	barplot(t1, las=2, legend.text=rownames(t1), col=rainbow(n = nrow(t1)), ylab = "", main = paste(Var,"by", var1, "and", var2), cex.names = graph_par$cex.x)
	title(ylab=y_title, line = graph_par$ylab_line)
	}
	
