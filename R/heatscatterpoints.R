heatscatterpoints <-
function(x, 						# x-vector
                             y, 						# y-vector
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
                             greyscale = FALSE,...) 	# should the colorpalette be in greyscales
{
if (!is.vector(x) | !is.vector(y)) stop("First two argument must be vectors !")
if (length(x) != length(y)) stop("Data vectors must be of the same length !")
if (!is.null(xlim)){cut = x >= xlim[1] & x <= xlim[2]
                    x = x[cut]
                    y = y[cut]
                    }
if (!is.null(ylim)){cut = y >= ylim[1] & y <= ylim[2]
                    y = y[cut]
                    x = x[cut]
                    }

getfrommat = function(a){d$z[a[1],a[2]]}
todiscrete = function(t,tmin,tmax,bins){
	erg = round((t-tmin)/(tmax-tmin)*bins+0.5)
	erg = pmin(pmax(erg,1),bins)
	return(erg)
	}

colpal = colorpalette(colpal,ncol)
if (greyscale){colpal = convertgrey(colpal)}
if (!is.null(alpha)){colpal = convertcolor(colpal,alpha)}
sound = which(isreal(x) & isreal(y))
if (length(sound)==0) stop("There are no valid point pairs to plot")
x = x[sound]
y = y[sound]

d <- kde2d(x,y,n=grid)

xdiscrete = todiscrete(x,min(x),max(x),bins=grid)
ydiscrete = todiscrete(y,min(y),max(y),bins=grid)
heatvec = unlist(apply(cbind(xdiscrete,ydiscrete),1,getfrommat))
coldiscrete = todiscrete(heatvec,min(d$z),max(d$z),bins=ncol)
points(x,y,col=colpal[coldiscrete],pch=pch,cex=cexplot,...)
if (add.contour){contour(d,add=TRUE,nlevels=nlevels,col=color.contour)}
}

