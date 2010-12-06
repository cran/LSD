kde2dplot <-
function(x,y, 				# a 2d density computed by kde2d
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

