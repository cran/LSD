

##########################################
### plot the rows of a matrix in lines ###
##########################################


plotmatrix = function(mat, 				# matrix
                      xlim = NULL, 		# xlimits, standard graphics parameter
                      ylim = NULL, 		# ylimits, standard graphics parameter
                      xlab = NULL, 		# xlabs, standard graphics parameter
                      ylab = NULL, 		# ylabs, standard graphics parameter
                      main = NULL, 		# title of the plot
                      type = "l", 		# type, standard plotting parameter
                      lwd = 2, 			# type, standard plotting parameter
                      at = NULL, 		# at which x-positions the columns should occur
                      xlabels = NULL, 	# text added as x-axis labels
                      ltys = NULL, 		# vector of linetypes
					  add = FALSE,		# add to existing plot
                      cols = NULL,...) 	# vector of colors
{
if (!is.matrix(mat)) stop("First argument must be a matrix !")
if (is.null(at)){at=1:ncol(mat)}
if (is.null(xlabels)){xlabels = at}
if (is.null(xlim)){xlim = c(0,max(at))}
if (is.null(ylim)){ylim = range(mat)}
if (is.null(main)){main = "Plotmatrix"}
if (is.null(ltys)){ltys = rep(1,nrow(mat))}
if (is.null(cols)) {cols = rep("black", nrow(mat))}else {if (length(cols) != nrow(mat)) {if(length(cols) < nrow(mat)){cols = rep(cols,ceiling(nrow(mat)/length(cols)))}
		cols = cols[1:nrow(mat)]
		cols[which(is.na(cols))] = "black"
	}
}
if(!add){plot(c(at[1],median(mat)),xlim=xlim,ylim=ylim,type="n",main=main,xaxt="n",ylab=ylab,xlab=xlab,...)}
axis(side=1,at=at,labels=xlabels)
sapply(1:nrow(mat),function(x){points(at,mat[x,],col=cols[x],lty=ltys[x],type=type,lwd=lwd)})
points(c(at[1],median(mat)),type = "n")
}


demo.plotmatrix = function()
{
LSDshow("plotmatrix")
devAskNewPage(ask = TRUE)
len = 20
x = sin(seq(0,2*pi,length=len*2))
fun = function(){n=sample(1:len,1); return(x[n:(n+len-1)])}
mat = t(replicate(7,fun(),simplify=T))
mat = mat + rnorm(length(mat))/2
plotmatrix(mat,cols=1:7,xlab="Columns of the matrix",ylab="Rows of the matrix")
}

#demo.plotmatrix()

