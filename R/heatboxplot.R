

#################################################################################################
### Heatboxplot: A boxplot with an additional color stripe based on a kernel density estimate ###
#################################################################################################


### densitylane ###


densitylane = function(x,y, 						# the density$x and density$y values of a density object
					pos = 1,						# position of the lane
					width = 0.4,					# width of the lane
					colpal = "standard",			# which colorpalette should be chosen ( see disco() )
					horizontal = horizontal, 		# should the density line be plotted horizontal
					alpha = NULL,					# alpha value for color opacity
					ncol = 75) 						# the number of colors to use
{
if (!is.vector(x) | !is.vector(y)) stop("First two argument must be vectors !")
if (length(x) != length(y)) stop("Data vectors must be of the same length !")
colpalette = colorpalette(colpal,ncol)
colpalette = paste(colpalette,alpha,sep="")
ycol  = pmin(pmax(round((y-min(y))/(max(y)-min(y))*ncol+0.5),1),ncol)
if (horizontal){ for (j in 1:(length(x)-1)){rect(x[j],pos-width/2,x[j+1],pos+width/2,col=colpalette[ycol[j]],border=NA)} }
else { for (j in 1:(length(x)-1)){rect(pos-width/2,x[j],pos+width/2,x[j+1],col=colpalette[ycol[j]],border=NA)} }
}


### heatboxplot ###


heatboxplot = function(x, 						# data as vector, matrix, list or data.frame
                       horizontal = FALSE, 		# should all be plotted horizontally
                       add = FALSE, 			# should the boxplot be added to an existing plot
			           colpal = "standard",     # which colorpalette should be chosen ( see disco() ), only if colpals = NULL
                       colpals = NULL, 			# which colorpalettes should be chosen ( see disco() )
					   ncol = 75, 				# the number of colors to use
                       lwd = 1.75, 				# linewidth to be used
                       axes = TRUE, 			# should the axes be plotted
                       labels = NULL, 			# labels of the boxplots
                       alpha = NULL, 			# alpha value for color opacity
                       xlim = NULL, 			# xlimits, standard graphics parameter
                       ylim = NULL, 			# ylimits, standard graphics parameter
                       xlab = NULL, 			# xlab, standard graphics parameter
                       ylab = "data values", 	# ylab, standard graphics parameter
                       main = "Heatboxplot", 	# title of the plot
                       nolab = FALSE, 			# should title and ylab be suppressed
					   outline = TRUE, 			# should outliers be plotted
					   boxonly = FALSE,        	# should the density only be plotted in the box
					   adjust = 1,				# scale the used bandwidth
					   quant.from = 0.25,		# from which quantile should the density lane be plotted
					   quant.to = 0.75,			# to which quantile should the density lane be plotted
					   range = 1.5,				# this determines how far the plot whiskers extend out from the box
					   border = "black",		# color of the box and whiskers
					   plot.boxplot = TRUE,		# should boxplot be added
					   add.quartiles = TRUE,    # box of the boxplot, if plot.boxplot = FALSE
					   add.box = FALSE,			# box of the plot
					   n.density = 1024,		# the number of equally spaced points at which the density is to be estimated
                       cexbox = 0.6,...) 		# cex of the boxes
{
	if (!is.vector(x) & !is.matrix(x) & !is.list(x) & !is.data.frame(x)) stop("x must be a vector, matrix, list or a data.frame !")
	if (!is.list(x) & !is.matrix(x) & !is.data.frame(x)){x = cbind(x)}
	if (is.data.frame(x)){x = as.list(x)}
	if (is.matrix(x)){x = as.list(as.data.frame(x))}
	if (is.null(colpals)){colpals = rep(colpal,length(x))}
	if (!is.null(xlim)){print("xlim argument will be ignored !")}
	if (is.null(ylim)){ylim = range(unlist(x))}
	if (!is.null(xlab)){print("xlab argument will be ignored ! Use labels instead !")}
	if (length(x) == 1){xlab = labels}
	if (length(x) == 1){cexbox = 0.4}
	if (length(x) > 1){boxwex = cexbox} else{boxwex = NULL}
	if (nolab){main = NULL
		ylab = NULL}
	if (horizontal){labdummy = ylab
		ylab = xlab
		xlab = labdummy}
	limlist = list()
	qlimlist = list()
	for (i in 1:length(x)){limlist[[i]] = c(quantile(x[[i]],quant.from),quantile(x[[i]],quant.to))}
	for (i in 1:length(x)){qlimlist[[i]] = c(quantile(x[[i]],0.25),quantile(x[[i]],0.75))}
	boxplot(x,border="white",add=add,horizontal=horizontal,axes=axes,xlim=c(0.5,length(x)+0.5),ylim=ylim,width=NULL,boxwex=boxwex,main=main,ylab=ylab,xlab=xlab,names=labels,outline=outline,range=range,lwd=lwd,...)
	for (j in 1:length(x)){xrel = x[[j]][x[[j]] > ylim[1] & x[[j]] < ylim[2]]
		dx = density(xrel,n=n.density,adjust=adjust)
		if(boxonly){dxx = dx$x[dx$x >= limlist[[j]][1] & dx$x <= limlist[[j]][2]]
			dxy = dx$y[dx$x >= limlist[[j]][1] & dx$x <= limlist[[j]][2]]}
		else {dxx = dx$x
			dxy = dx$y}
		densitylane(dxx,dxy,colpal=colpals[[j]],horizontal=horizontal,pos=j,width=cexbox,ncol=ncol,alpha=alpha)}
	if (plot.boxplot){boxplot(x,add=TRUE,horizontal=horizontal,axes=axes,col="transparent",xlim=c(0.5,length(x)+0.5),ylim=ylim,width=NULL,boxwex=boxwex,names=labels,outline=outline,range=range,lwd=lwd,...)}                                 
	if (!plot.boxplot){if (add.quartiles) {if (horizontal){ for (j in 1:length(x)){rect(qlimlist[[j]][1],j-cexbox/2,qlimlist[[j]][2],j+cexbox/2,border="grey40",lwd=lwd)} }
			else { for (j in 1:length(x)){rect(j-cexbox/2,qlimlist[[j]][1],j+cexbox/2,qlimlist[[j]][2],border="grey40",lwd=lwd)} }}}
	if (add.box){if (horizontal){ for (j in 1:length(x)){rect(limlist[[j]][1],j-cexbox/2,limlist[[j]][2],j+cexbox/2,border=border,lwd=lwd)} }
		else { for (j in 1:length(x)){rect(j-cexbox/2,limlist[[j]][1],j+cexbox/2,limlist[[j]][2],border=border,lwd=lwd)} }}
}


### demo.heatboxplot ###


demo.heatboxplot = function()
{
	par(mfrow=c(1,1))
	LSDshow("heatboxplot")
	devAskNewPage(ask = TRUE)
	f = c(rnorm(200),rnorm(200)+4)
	h = rf(500,15,15)*10
	g = rnorm(300)+1
	heatboxplot(h,labels=c("unimodal"))
	devAskNewPage(ask = TRUE)
	heatboxplot(list(f=f,g=g),colpals=c("rdpu","greens"),main="rdpu and greens",labels=c("bimodal","unimodal"))
	devAskNewPage(ask = TRUE)
	heatboxplot(list(f=f,g=g),colpals=c("rdpu","greens"),main="rdpu and greens with adjust = 0.2",labels=c("bimodal","unimodal"),adjust=0.2)
	devAskNewPage(ask = TRUE)
	par(mai = c(1.02,1.02,0.82,0.42))
	heatboxplot(list(f=f,g=g),colpals=c("rdpu","greens"),horizontal=TRUE,main="rdpu and greens with horizontal = TRUE",labels=c("bimodal","unimodal"),las=2)
	devAskNewPage(ask = TRUE)
	l = list()
	for (i in 1:30){l[[i]] = rnorm(200,mean=sqrt(i^2.5),sd=1+i/2)}
	heatboxplot(l,colpal="blues",main="blues",boxonly=TRUE,adjust=0.5,quant.from=0.05,quant.to=0.95,plot.boxplot=FALSE,cexbox=1,add.quartiles=FALSE)
	legend("topleft",c("colpal = \"blues\"","boxonly = TRUE","adjust = 0.5","quant.from = 0.05","quant.to = 0.95","plot.boxplot = FALSE","cexbox = 1","add.quartiles = FALSE"),inset=0.01)
}


#demo.heatboxplot()



