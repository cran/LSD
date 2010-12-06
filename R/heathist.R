heathist <-
function(x, 							# vector
                    breaks = 20, 				# breaks of the histogram
                    main = "Heat-histogram", 	# title of the plot
                    colpal = "greys", 	    	# which colorpalettes should be chosen ( see disco() )
                    nobox = FALSE, 				# should the box be plotted
                    revpal = FALSE, 			# reverse the colorpalette
					add.density = FALSE,      	# add density line
					col.density = "darkred", 	# color of the density line
					add.rug = TRUE,             # add a rug (1-d plot of the data) below the histogram-bars
					col.rug = "black", 			# color of the rug
                    ncolors = 100,...) 			# number of colors to use
{
if (!is.vector(x)) stop("x must be a vector !")
xhist = hist(x,plot = FALSE,breaks = breaks)
d = density(x)
plot(xhist,border = NA,freq = FALSE,main = main,...)
usr = par("usr")
dy = (max(xhist$density) - 0)/ncolors
colpal  = colorpalette(colpal,ncolors)
if (revpal){colpal = rev(colpal)}
# for each color, clip into a region and redraw the histogram with that color
for(i in 1:ncolors){
 clip(usr[1],usr[2],0 + (i-1) * dy,0 + i*dy)
 plot(xhist,add = TRUE,axes = FALSE,col = colpal[i],border = NA,freq = FALSE,xlab = "",ylab = "",main = "")
}
# reset the clipping area. See ?clip
do.call(clip,as.list(usr))
# just to get the boxes right
plot(xhist,add = TRUE,lwd = .5 ,freq = FALSE,axes = FALSE,xlab = "",ylab = "",main = "")
if (add.density){lines(d,lwd = 4,col = col.density)}
rug(x,col = col.rug)
if (!nobox){box()}
}

