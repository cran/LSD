densitylane <-
function(x,y, 						# the density$x and density$y values of a density object
					pos = 1,						# position of the lane
					width = 0.4,					# width of the lane
					colpal = "standard",			# which colorpalette should be chosen ( see disco() )
					horizontal = horizontal, 		# should the density line be plotted horizontal
					alpha = NULL,					# alpha value for color opacity
					ncol = 75) 						# the number of colors to use
{
if (!is.vector(x) | !is.vector(y)) stop("First two argument must be vectors !")
if (length(x) != length(y)) stop("Data vectors must be of the same length !")
colpalette = colorpalette(colpal,ncol)
colpalette = paste(colpalette,alpha,sep="")
ycol  = pmin(pmax(round((y-min(y))/(max(y)-min(y))*ncol+0.5),1),ncol)
if (horizontal){ for (j in 1:(length(x)-1)){rect(x[j],pos-width/2,x[j+1],pos+width/2,col=colpalette[ycol[j]],border=NA)} }
else { for (j in 1:(length(x)-1)){rect(pos-width/2,x[j],pos+width/2,x[j+1],col=colpalette[ycol[j]],border=NA)} }
}

