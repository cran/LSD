

################################################################################################################################################
### Heatscatterplot: A colored scatterplot based on a 2d density computed by kde2d. Applications: heatpairs, heat-MA-plots and heat-MA-pairs ###
################################################################################################################################################


### the kde2dplot routine is a function from the R Graph Gallery ###


kde2dplot = function(x,y, 				# a 2d density computed by kde2d
                     grid = 100, 		# size of the grid
                     ncol = 30, 		# the number of colors to use
                     nlevels = 10, 		# number of levels
                     main = "kde2d") 	# title of the plot
{
if (!is.vector(x) | !is.vector(y)) stop("First two argument must be vectors !")
if (length(x) != length(y)) stop("Data vectors must be of the same length !")
d = kde2d(x,y,n=grid)
z   <- d$z
nrz <- nrow(z)
ncz <- ncol(z)
couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol)
par(mar=c(2,2,2,2))
image(d,col=couleurs,main=main)
contour(d,add=TRUE,nlevels=nlevels)
box()
}


### heatscatter ###


heatscatter = function(x, 						# x-vector
                       y, 						# y-vector
                       pch = 19, 				# plotting character
                       cexplot = 0.5, 			# cex of the points
                       ncol = 30, 				# the number of colors to use
                       grid = 100, 				# size of the grid
                       colpal = "heat", 		# which colorpalette should be chosen ( see disco() )
                       xlim = NULL, 			# xlimits, standard graphics parameter
                       ylim = NULL, 			# ylimits, standard graphics parameter
                       main = NULL, 			# the title of the plot
                       cor = TRUE, 				# should the correlation be added to the title
					   method = "spearman",     # correlation method    
                       alpha = NULL, 			# alpha value for color opacity
					   add.contour = FALSE,     # should the contour lines be added
					   nlevels = 10,			# number of levels of the contour lines
					   color.contour = "black", # color of the contour lines
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

if (cor){if (is.null(main)){main = paste("Heatscatter  cor = ",round(cor(x,y,method=method),digits=2))}
         else {main = paste(main," cor = ",round(cor(x,y,method=method),digits=2))}}
else {if (is.null(main)){main = "Heatscatter"}
      else {main = main}}

d <- kde2d(x,y,n=grid)

xdiscrete = todiscrete(x,min(x),max(x),bins=grid)
ydiscrete = todiscrete(y,min(y),max(y),bins=grid)
heatvec = unlist(apply(cbind(xdiscrete,ydiscrete),1,getfrommat))
coldiscrete = todiscrete(heatvec,min(d$z),max(d$z),bins=ncol)
plot(x,y,col=colpal[coldiscrete],pch=pch,cex=cexplot,xlim=xlim,ylim=ylim,main=main,...)
if (add.contour){contour(d,add=TRUE,nlevels=nlevels,col=color.contour)}
}


### demo.heatscatter ###


demo.heatscatter = function()
{
par(mfrow=c(1,1))
LSDshow("heatscatter")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.8)
x = sign(x)*abs(x)^1.3
par(mfrow=c(2,2))
kde2dplot(x,y,main="Density Estimation (from R Graph Gallery)")
plot(x,y,pch=19,col="#00000010",main="Alpha Scatter")
heatscatter(x,y,colpal="heat")
heatscatter(x,y,colpal="heat",greyscale = TRUE,alpha=25)
devAskNewPage(ask = TRUE)
par(mfrow=c(2,2))
heatscatter(x,y,colpal="jamaica",main="jamaica",cor=FALSE)
heatscatter(x,y,colpal="bl2gr2rd",main="bl2gr2rd",cor=FALSE)
heatscatter(x,y,colpal="heat",main="greyscales with add.contour=TRUE",cor=FALSE,,add.contour=TRUE,color.contour="red",greyscale=TRUE)
heatscatter(x,y,colpal="spectral",main="spectral with add.contour=TRUE",cor=FALSE,add.contour=TRUE)
}

#demo.heatscatter()


### heatscatterpoints ###


heatscatterpoints = function(x, 						# x-vector
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


### heatpairs ###


heatpairs = function(mat, 						# matrix
                     main = "Heatpairs", 		# the title of the plot
                     xlim = NULL, 				# xlimits, standard graphics parameter
                     ylim = NULL, 				# ylimits, standard graphics parameter
					 labels = NULL,			    # labels to be shown on the diagonal
					 add.points = FALSE,		# should a certain group of points be colored somehow
					 group = NULL, 				# certain group of points that should be highlighted
					 color.group = "magenta",   # color in which the certain group of points should be highlighted
					 method = "spearman",     	# correlation method
					 colpal = "heat", 			# which colorpalette should be chosen ( see disco() )
					 pch=19, 					# plotting character
					 cexplot=0.5, 				# cex of the points
					 ncol=30, 					# the number of colors to use
					 grid=100, 					# size of the grid
					 alpha = NULL, 				# alpha value for color opacity
					 add.contour = FALSE,     	# should the contour lines be added
					 nlevels = 10,				# number of levels of the contour lines
					 color.contour = "black", 	# color of the contour lines
					 greyscale = FALSE,...) 	# should the colorpalette be in greyscales
{
if (!is.matrix(mat)) stop("First argument must be a matrix !")
if (is.null(xlim)){xlim = c(min(mat),max(mat))}
if (is.null(ylim)){ylim = c(min(mat),max(mat))}
if(is.null(labels)){labels = colnames(mat)}
pairs(mat,labels=labels,xlim=xlim,ylim=ylim,lower.panel=function(x,y,...){text(sum(xlim)/2,sum(ylim)/2,round(cor(x,y,method=method),digits=2),cex = 3)},main=main,upper.panel=function(x,y,...){heatscatterpoints(x,y,colpal=colpal,pch=pch,cexplot=cexplot,ncol=ncol,grid=grid,alpha=alpha,add.contour=add.contour,nlevels=nlevels,color.contour=color.contour,greyscale=greyscale,...);abline(a=0,b=1);if (add.points){points(x[rownames(mat) %in% group],y[rownames(mat) %in% group],col=color.group,...)}},...)
}


### demo.heatscatterpoints ###


demo.heatscatterpoints = function()
{
par(mfrow=c(1,1))
LSDshow("heatscatterpoints")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.75)
x = sign(x)*abs(x)^1.3
mat = cbind(x,y,x + rnorm(points,sd=0.5))
colnames(mat) = c("x","y","z")
rownames(mat) = 1:nrow(mat)
heatpairs(mat,main="Heatpairs",method="spearman",labels=c(expression(Xi),expression(Lambda),expression(Delta)))
devAskNewPage(ask = TRUE)
heatpairs(mat,main="Heatpairs in greyscales with add.contour=TRUE",method="pearson",add.contour=TRUE,color.contour="red",greyscale=TRUE,labels=c(expression(Xi),expression(Lambda),expression(Delta)))
}


### demo.heatpairs ###


demo.heatpairs = demo.heatscatterpoints

#demo.heatscatterpoints()


### heatmaplot ###


heatmaplot = function(x, 						# x-vector
					y, 							# y-vector
					pch = 19, 					# plotting character
					cexplot = 0.5, 				# cex of the points
					ncol = 30, 					# the number of colors to use
					grid = 100, 				# size of the grid
					colpal = "heat", 			# which colorpalette should be chosen ( see disco() )
					xlim = NULL, 				# xlimits, standard graphics parameter
					ylim = NULL, 				# ylimits, standard graphics parameter
					xlab = "M-values", 			# xlab, standard graphics parameter
					ylab = "A-values", 			# ylab, standard graphics parameter
					main = "heatmaplot", 		# the title of the plot
					cor = FALSE, 				# should the (spearman) correlation be added to the title
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
heatscatter(a,m,pch=pch,cexplot=cexplot,ncol=ncol,grid=grid,colpal=colpal,xlim=xlim,ylim=ylim,main=main,cor=cor,alpha=alpha,add.contour=add.contour,nlevels=nlevels,color.contour=color.contour,greyscale=greyscale,...)
if (add.line){abline(h=0,col=color.line,lwd=lwd)}
}


### demo.heatmaplot ###


demo.heatmaplot = function()
{
par(mfrow=c(1,1))
LSDshow("heatmaplot")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.2)
x = sign(x)*abs(x)^1.25
par(mfrow=c(2,2))
heatmaplot(x,y,colpal="heat",main="heat",cor=FALSE)
heatmaplot(x,y,colpal="bl2gr2rd",main="bl2gr2rd",cor=FALSE)
heatmaplot(x,y,colpal="heat",main="greyscales with add.contour=TRUE",cor=FALSE,,add.contour=TRUE,color.contour="red",greyscale=TRUE)
heatmaplot(x,y,colpal="spectral",main="spectral with add.contour=TRUE",cor=FALSE,add.contour=TRUE)
}

#demo.heatmaplot()


### heatmapoints ###


heatmapoints = function(x, 							# x-vector
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


### heatmapairs ###


heatmapairs = function(mat, 					# matrix
					main = "Heat MA pairs", 	# the title of the plot
					xlim = NULL, 				# xlimits, standard graphics parameter
					ylim = NULL, 				# ylimits, standard graphics parameter
					labels = NULL,			    # labels to be shown on the diagonal
					add.points = FALSE,			# should a certain group of points be colored somehow
					group = NULL, 				# certain group of points that should be highlighted
					color.group = "magenta",    # color in which the certain group of points should be highlighted
					method = "spearman",     	# correlation method
					colpal = "heat", 			# which colorcolpal should be chosen ( see disco() )
					pch=19, 					# plotting character
					cexplot=0.5, 				# cex of the points
					ncol=30, 					# the number of colors to use
					grid=100, 					# size of the grid
					alpha = NULL, 				# alpha value for color opacity
					add.contour = FALSE,     	# should the contour lines be added
					nlevels = 10,				# number of levels of the contour lines
					color.contour = "black", 	# color of the contour lines
					greyscale = FALSE, 			# should the colorpalette be in greyscales
					add.line = TRUE,			# add horizontal line to the plot
					color.line = "darkgrey",	# color of the horizontal line
					lwd = 2,...)				# lwd, standard graphics parameter
{
if (!is.matrix(mat)) stop("First argument must be a matrix !")
if (is.null(xlim)){xlim = c(min(mat),max(mat))}
if (is.null(ylim)){ylim = c(min(mat),max(mat))}
if(is.null(labels)){labels = colnames(mat)}
mapoints = function(x,y,...){points((x + y)/2,x - y,...)}
pairs(mat,labels=labels,xlim=xlim,ylim=ylim,lower.panel=function(x,y,...){text(sum(xlim)/2,sum(ylim)/2,round(cor(x,y,method=method),digits=2),cex = 3)},main=main,upper.panel=function(x,y,...){heatmapoints(x,y,colpal=colpal,pch=pch,cexplot=cexplot,ncol=ncol,grid=grid,alpha=alpha,add.contour=add.contour,nlevels=nlevels,color.contour=color.contour,greyscale=greyscale,...);if (add.line){abline(h=0,col=color.line,lwd=lwd)};if (add.points){mapoints(x[rownames(mat) %in% group],y[rownames(mat) %in% group],col=color.group,...)}},...)
}


### demo.heatmapoints ###


demo.heatmapoints = function()
{
par(mfrow=c(1,1))
LSDshow("heatmapoints")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.75)
x = sign(x)*abs(x)^1.25
mat = cbind(x,y,x + rnorm(points,sd=0.5))
rownames(mat) = 1:nrow(mat)
colnames(mat) = c("x","y","z")
heatmapairs(mat,main="Heatmapairs",method="spearman",labels=c(expression(Theta),expression(Phi),expression(Omega)))
devAskNewPage(ask = TRUE)
heatmapairs(mat,main="Heatmapairs in greyscales with add.contour=TRUE",method="pearson",add.contour=TRUE,color.contour="red",greyscale=TRUE,labels=c(expression(Theta),expression(Phi),expression(Omega)))
}


### demo.heatmapairs ###


demo.heatmapairs = demo.heatmapoints

#demo.heatmapoints()

