boxplot_var_by_one_var <- function(x,  Var, var1)
	{
	# prepares a barplot of Var per var1
	# Nuno Prista @ RCM NS&EA 2017
	
	# e.g.: 
		# CA:
			# boxplot_var_by_one_var(x = data1$caSub[data1$caSub$Age!="NULL",], Var = "Age" , var1 = "FlagCountry")
		
	windows(10,7); par(cex=0.8, oma = c(1,1,1,1))
	y_title = Var
	boxplot(as.numeric(as.character(x[, Var]))~x[, var1], las=2, col=rainbow(n = nlevels(x[,var1])),ylab = y_title, main = paste(Var,"by", var1))
	}
	
	
