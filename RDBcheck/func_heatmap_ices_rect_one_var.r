heatmap_ices_rect_one_var<-function(x, Var, F, map.title, legend.title, nbreaks, xlim = c(-4.5,+14.5), ylim = c(49.75,62.25), legend.amp=1, map_file =""){
# draws grid map with heat-mapped ices rectangles
# x is a subset of data (with ICES_RECT= ices rectangles). 
# F is the function that is applied to on each ices rectangle (e.g., sum, mean)
# xlim and ylim help define map range
# legend.title and legend.amp are for legend only. Note: use legend.amp = 100 if data is percentage

# Nuno Prista @ RCM NS&EA 2017

# Example
	#heatmap_ices_rect_one_var(x=data1$ceSub, Var = "KWDays", F = sum, map.title = "sum KWDays", xlim=c(-4.5,+14.5), ylim=c(49.75,62.25), legend.title="sum KWDays", nbreaks=6, map_file ="001_Inputs/GSHHS_l_L1.shp"))

require(rgdal)
require(mapplots)

map<-readOGR(map_file)	
	
#builds dataset per ICES_RECT
ls1<-split(x, factor(x$StatisticalRectangle))
ls2<-lapply(ls1, function(x){data.frame(StatisticalRectangle = x$StatisticalRectangle[1], value = F(x[[Var]]))})
df1<-do.call("rbind",ls2)
df1<-data.frame(df1, ices.rect(df1$StatisticalRectangle))

# map
# graph parameters
byx = 1; byy = 0.5;
grd<-make.grid(df1$lon, df1$lat, df1$value, byx, byy , xlim, ylim)
breaks <- breaks.grid(grd, quantile=1, ncol=nbreaks, zero=TRUE)
print(breaks)

# map
basemap(xlim=xlim, ylim=ylim, main = map.title)
draw.rect()
draw.grid(grd,breaks)
par(new=T);plot(map, xlim=xlim, ylim=ylim, bg=FALSE, col="gray")
legend.grid("topright", breaks=breaks*legend.amp, type=2, inset=0.02, title=legend.title)
}
	
