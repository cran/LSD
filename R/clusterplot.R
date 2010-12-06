clusterplot <-
function(clusmat, 									# matrix with numerical entries, quantiles of rows will define colors
                       label = NULL, 								# if multiple clusters should be plotted: vector with labels, which assigns rows to clusters
                       at = NULL, 									# at which x-positions the columns should occur
                       main = "Clusterplot", 						# the title(s) of the plot(s)
                       xlim = NULL, 								# xlimits, standard graphics parameter
                       ylim = NULL, 								# ylimits, standard graphics parameter
                       xlab = "Columns",							# x-axis legend, standard graphics parameter
                       ylab = "Rows",								# y-axis legend, standard graphics parameter 
                       xaxt = "s", 									# should an x axis be plotted at all? (="n" if not)
                       xlabels = NULL, 								# text added as x-axis labels
                       las = 1, 									# las=1: horizontal text, las=2: vertical text (x-axis labels)
                       fromto = c(0.05,0.95), 						# the range of quantiles that should be plotted
                       colpal = "standardheat",	 					# which colorpalette should be chosen ( see disco() )
                       nrcolors = 25, 								# the number of colors to use
                       outer.col = "light grey", 					# color of the outlier lines
					   quartiles.col = c("black","grey","grey"), 	# color of quartile lines
                       add.quartiles = TRUE, 						# should the quartile lines be plotted
                       separate = TRUE, 							# should clusters be plotted in subsequent figures
                       revpal = FALSE, 								# should the colorpalette be inverted
                       size = TRUE, 								# should the size of each cluster be added to the title
                       alpha = NULL,...) 							# alpha value for color opacity
{
if (!is.matrix(clusmat)) stop("First argument must be a matrix !")
if (is.null(at)) at=1:ncol(clusmat)
probes = length(at)
if (is.null(xlim)) xlim=c(min(at),max(at))
maxp = xlim[2]
minp = xlim[1]
if (is.null(ylim)) ylim=c(min(clusmat),max(clusmat))
if (is.null(xlabels)) xlabels = 1:length(at)
# one cluster in one plot
if (is.null(label)){if (size){main = paste(main,"#",nrow(clusmat))}
                    plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=main,...)
                    if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels,...)
                    singleclusterplot(clusmat=clusmat,at=at,fromto=fromto,colpal=colpal,nrcolors=nrcolors,outer.col=outer.col,add.quartiles=add.quartiles,quartiles.col=quartiles.col,revpal=revpal,alpha=alpha)}
# several clusters in one plot or several plots
if (!is.null(label)){clusternames = sort(unique(label))
                     nrclusters = length(clusternames)
                     clustersets = split(1:nrow(clusmat),factor(label))
                     if (!is.list(colpal)) colpal = as.list(colpal)
                     if (length(colpal)<nrclusters) colpal = rep(colpal,nrclusters)	
                     if (length(main)>0) main = paste(main,"\n",sep="")
                     if (separate==FALSE){if (size){main = paste(main,"#",nrow(clusmat))}
                                          plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=main,...)
                                          if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels,...)}
                     if (separate==TRUE) par(mfrow=windowxy(nrclusters))
                     for (j in seq(clusternames)){if (separate==TRUE){if (length(main)==1) clustermain = paste(main,clusternames[j]," # ",length(clustersets[[j]]),sep="") else clustermain = main[j]
                                                                      plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=clustermain,...)
                                                                      if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels,...)}
                                                  singleclusterplot(clusmat=clusmat[clustersets[[j]],,drop=FALSE],at=at,fromto=fromto,colpal=colpal[[j]],nrcolors=nrcolors,outer.col=outer.col,add.quartiles=add.quartiles,quartiles.col=quartiles.col,revpal=revpal,alpha=alpha)
                                                 }
                    }
}

