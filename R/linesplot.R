linesplot <-
function(x, 						# data as vector, matrix, list or data.frame
                     labels = NULL, 			# vector of labels
                     col = "black", 			# standard color
					 cols = NULL, 				# vector of colors
                     alpha = 25, 				# alpha value for color opacity
                     xlim = NULL, 				# xlimits, standard graphics parameter
                     ylim = NULL, 				# ylimits, standard graphics parameter
                     xlab = NULL, 				# xlab, standard graphics parameter
                     ylab = "data values", 		# ylab, standard graphics parameter
					 las = 1,                   # las=1: horizontal text, las=2: vertical text (x-axis labels)
					 outline = TRUE, 			# should outliers be plotted
					 cexbox = 0.6,		 		# cex of the boxes
					 addboxes = FALSE, 			# should boxplots be added
					 border = "black",			# color of the box and whiskers
					 range = 1.5,				# this determines how far the plot whiskers extend out from the box
					 lwd = 1.5,					# linewidth of the box and whiskers
                     main = "Linesplot",...) 	# title of the plot
{
if (!is.vector(x) & !is.matrix(x) & !is.list(x) & !is.data.frame(x)) stop("x must be a vector, matrix, list or a data.frame !")
if (!is.list(x) & !is.matrix(x) & !is.data.frame(x)){x = cbind(x)}
if (is.data.frame(x)){x = as.list(x)}
if (is.matrix(x)){x = as.list(as.data.frame(x))}
if (is.null(labels)){labels = labels(x)}
if (is.null(cols)){cols = rep(convertcolor(col,alpha),length(x))} else{cols = convertcolor(cols,alpha)}
if (!is.null(xlim)){print("xlim argument will be ignored !")}
if (is.null(ylim)){ylim = range(unlist(x))}
if (!is.null(xlab)){print("xlab argument will be ignored ! Use labels instead !")}
if (length(x) == 1){cexbox = 0.4}
if (length(x) > 1){boxwex = cexbox} else{boxwex = NULL}
plot(1,col="white",xlim=c(0.5,length(x)+0.5),ylim=ylim,xlab="",xaxt="n",ylab=ylab,main=main,...)
par.axis.default = par("cex.axis")
par(cex.axis = par("cex.lab"))
axis(1,at=seq(1,length(x),1),labels=labels,las=las)
for (j in 1:length(x)){for (i in x[[j]]){lines(c(j-cexbox/2,j+cexbox/2),c(i,i),lwd=2,col=cols[j])}}
if (addboxes){boxplot(x,add=TRUE,col="transparent",xlim=c(0.5,length(x)+0.5),ylim=ylim,width=NULL,boxwex=boxwex,outline=outline,axes=FALSE,border=border,lwd=lwd,range=range)}                                 
par(cex.axis = par.axis.default)                                 
}

