piechart <-
function(props, 				# vector giving the relations of the pie pieces (need not to be normed)
                    x = 0, 				# x-position of the piechart
                    y = 0, 				# y-position of the piechart
                    radius = 1, 		# radius of the piechart
                    cols = NULL, 		# vector of colors
                    colpal = "pies", 	# colorpalette if cols is not specified
                    edges = 100, 		# number of edges the "circle" will have
                    add = FALSE, 		# should piechart be added to an existing plot
                    xlim = NULL, 		# xlimits, standard graphics parameter
                    ylim = NULL, 		# ylimits, standard graphics parameter
                    main = "Piecharts", # title of the plot
                    alpha = NULL,...) 	# alpha value for color opacity
{
xx = sin(-(0:edges)/edges*2*pi)*radius
yy = cos(-(0:edges)/edges*2*pi)*radius
n = length(props)
if (is.null(cols)) cols = colorpalette(colpal,n,alpha)	
phi = cumsum(props)/sum(props)*edges
phi = c(0,round(phi))+1
if (!add){emptyplot(xlim=xlim,ylim=ylim,main=main,...)}	
for (j in 1:n){xvec = c(x,x+xx[phi[j]:phi[j+1]])
		           yvec = c(y,y+yy[phi[j]:phi[j+1]])
               polygon(xvec,yvec,col=cols[j],border=NA)
              }
}

