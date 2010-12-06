comparisonplot <-
function(x, 						# x-vector
                          y, 						# y-vector
                          histbreaks = 30, 			# breaks of the histograms
						  adjust = 1,				# scale the used bandwidth ( see demo.heatboxplot() )
                          colpal="heat", 			# which colorpalette should be chosen ( see disco() )
                          main = NULL, 				# maintitle of the plot
                          xlim = NULL, 				# xlimits, standard graphics parameter
                          ylim = NULL, 				# ylimits, standard graphics parameter
                          ab = FALSE, 				# should an abline be added to the plot
						  add.density = FALSE,      # add density line
						  col.density = "darkred", 	# color of the density line
                          pimp = FALSE,...) 		# PIMP MY PLOT
{
if (!is.vector(x) | !is.vector(y)) stop("First two argument must be vectors !")
sound = which(isreal(x) & isreal(y))
if (length(sound)==0) stop("There are no valid point pairs to plot")
x = x[sound]
y = y[sound]
xrange <- c(min(range(x),range(y)),max(range(x),range(y)))
yrange <- c(min(range(x),range(y)),max(range(x),range(y)))
if (!is.null(xlim)){cut = x > xlim[1] & x < xlim[2]
                    x = x[cut]
                    y = y[cut]
                    xrange = xlim
                    }
if (!is.null(ylim)){cut = y > ylim[1] & y < ylim[2]
                    y = y[cut]
                    x = x[cut]
                    yrange = ylim
                    }                           
def.par = par(no.readonly = TRUE) # save default, for resetting...
xhist = hist(x, breaks=seq(xrange[1],xrange[2],length.out=histbreaks),plot=FALSE)
yhist = hist(y, breaks=seq(yrange[1],yrange[2],length.out=histbreaks),plot=FALSE)
top = max(c(xhist$density,yhist$density))
nf = layout(matrix(c(0,2,0,4,1,3,0,5,0),3,3,byrow=TRUE), c(1,3,1), c(1,3,1), TRUE)
dx = density(x)
dy = density(y)
if (is.null(main)){main = "Comparisonplot"}
par(mar=c(4,4,4,4))
heatscatter(x,y,xlim=xrange,ylim=yrange,main=main,colpal=colpal,...)
if (pimp){
if (ab){abline(0,1,col="#08306B50",lwd=2)}
par(mar=c(0,4,1,4))
heatbarplot(xhist,ylim=c(0,top),colpal=colpal)
if (add.density){lines((dx$x-xhist$breaks[1])/(xhist$breaks[2]-xhist$breaks[1]),dx$y,lwd = 2,col = col.density)}
par(mar=c(4,0,4,1))
heatbarplot(yhist,xlim=c(0,top),colpal=colpal,horiz=TRUE)
if (add.density){lines(dy$y,(dy$x-yhist$breaks[1])/(yhist$breaks[2]-yhist$breaks[1]),lwd = 2,col = col.density)}
par(mar=c(4,1,4,0))
heatboxplot(y,axes=FALSE,graphics = FALSE,colpal=colpal,ylim=yrange,nolab=TRUE,adjust=adjust)
par(mar=c(1,4,0,4))
heatboxplot(x,axes=FALSE,horizontal=TRUE,graphics = FALSE,colpal=colpal,ylim=xrange,nolab=TRUE,adjust=adjust)
}
else {
if (ab){abline(0,1,col="black",lwd=1)}
par(mar=c(0,4,1,4))
barplot(xhist$density, axes=FALSE, ylim=c(0, top), space=0,col="#F0F0F0")
if (add.density){lines((dx$x-xhist$breaks[1])/(xhist$breaks[2]-xhist$breaks[1]),dx$y,lwd = 2,col = col.density)}
par(mar=c(4,0,4,1))
barplot(yhist$density, axes=FALSE, xlim=c(0, top), space=0,col="#F0F0F0", horiz=TRUE)
if (add.density){lines(dy$y,(dy$x-yhist$breaks[1])/(yhist$breaks[2]-yhist$breaks[1]),lwd = 2,col = col.density)}
par(mar=c(4,1,4,0))
boxplot(y,axes=FALSE,col="#F0F0F0",ylim=yrange)
par(mar=c(1,4,0,4))
boxplot(x,axes=FALSE,horizontal=TRUE,col="#F0F0F0",ylim=xrange)
}
par(def.par)
}

