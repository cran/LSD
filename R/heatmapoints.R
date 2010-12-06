heatmapoints <-
function(x, 							# x-vector
						y, 							# y-vector
						pch=19, 					# plotting character
						cexplot=0.5, 				# cex of the points
						ncol=30, 					# the number of colors to use
						grid=100, 					# size of the grid
						colpal = "heat", 			# which colorpalette should be chosen ( see disco() )
						xlim = NULL, 				# xlimits, standard graphics parameter
						ylim = NULL, 				# ylimits, standard graphics parameter
						alpha = NULL, 				# alpha value for color opacity
						add.contour = FALSE,     	# should the contour lines be added
						nlevels = 10,				# number of levels of the contour lines
						color.contour = "black", 	# color of the contour lines
						add.line = TRUE,			# add horizontal line to the plot
						color.line = "darkgrey",	# color of the horizontal line
						lwd = 2,					# lwd, standard graphics parameter
						greyscale = FALSE,...) 		# should the colorpalette be in greyscales
{
if (!is.vector(x) | !is.vector(y)) stop("First two argument must be vectors !")
if (length(x) != length(y)) stop("Data vectors must be of the same length !")
m = x - y
a = (x + y)/2
heatscatterpoints(a,m,pch=pch,cexplot=cexplot,ncol=ncol,grid=grid,colpal=colpal,xlim=xlim,ylim=ylim,alpha=alpha,add.contour=add.contour,nlevels=nlevels,color.contour=color.contour,greyscale=greyscale,...)
if (add.line){abline(h=0,col=color.line,lwd=lwd)}
}

