heatbarplot <-
function(x, 					# histogram object
                       colpal = "black", 	# which colorpalette should be chosen ( see disco() )
                       revpal = FALSE, 		# reverse the colorpalette
                       horiz = FALSE, 		# rotate 90 degrees
                       ncolors = 100,...) 	# number of colors to use
{
if (class(x) != "histogram") stop("x must be of class histogram !")
if (!horiz){
barplot(x$density,axes = FALSE,space = 0,border = "white",...)
dy = (max(x$density) - 0)/ncolors
colpal = colorpalette(colpal,ncolors)
if (revpal){colpal = rev(colpal)}
colorlane = function(a,b){for(i in 1:100){rect(a,0 + (i-1) * dy,b,0 + i*dy,col = colpal[i],border = NA)}}
for (j in 1:length(x$counts)){colorlane(j-1,j)}
for (j in 1:length(x$counts)){rect(j-1,x$density[j],j,max(x$density),col = "white",border = NA)}
for (j in 1:length(x$counts)){rect(j-1,0,j,x$density[j],col = "transparent",border = NULL)}
} else{
barplot(x$density,axes = FALSE,space = 0,border = "white",horiz=TRUE,...)
dy = (max(x$density) - 0)/ncolors
colpal = colorpalette(colpal,ncolors)
if (revpal){colpal = rev(colpal)}
colorlane = function(a,b){for(i in 1:100){rect(0 + (i-1) * dy,a,0 + i*dy,b,col = colpal[i],border = NA)}}
for (j in 1:length(x$counts)){colorlane(j-1,j)}
for (j in 1:length(x$counts)){rect(x$density[j],j-1,max(x$density),j,col = "white",border = NA)}
for (j in 1:length(x$counts)){rect(0,j-1,x$density[j],j,col = "transparent",border = NULL)}}
}

