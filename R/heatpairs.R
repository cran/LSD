heatpairs <-
function(mat, 						# matrix
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
					 greyscale = FALSE, 		# should the colorpalette be in greyscales
                     cexlabels = 3) 			# cex of the labels
{
if (!is.matrix(mat)) stop("First argument must be a matrix !")
if (is.null(xlim)){xlim = c(min(mat),max(mat))}
if (is.null(ylim)){ylim = c(min(mat),max(mat))}
if(is.null(labels)){labels = colnames(mat)}
pairs(mat,labels=labels,xlim=xlim,ylim=ylim,cex.labels = cexlabels,lower.panel=function(x,y,...){text(sum(xlim)/2,sum(ylim)/2,round(cor(x,y,method=method),digits=2),cex = 3)},main=main,upper.panel=function(x,y,...){heatscatterpoints(x,y,colpal=colpal,pch=pch,cexplot=cexplot,ncol=ncol,grid=grid,alpha=alpha,add.contour=add.contour,nlevels=nlevels,color.contour=color.contour,greyscale=greyscale,...);abline(a=0,b=1);if (add.points){points(x[rownames(mat) %in% group],y[rownames(mat) %in% group],col=color.group,...)}})
}

