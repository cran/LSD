msdplot <-
function(x, 								# matrix with numerical entries, quantiles of rows will define lines
                   label = NULL, 					# vector with labels, if multiple clusters should be plotted
                   at = NULL, 						# at which x-positions the columns should occur
                   xlim = NULL, 					# xlimits, standard graphics parameter
                   ylim = NULL, 					# ylimits, standard graphics parameter
                   xlab = "timepoints", 			# xlab, standard graphics parameter
                   ylab = "values", 				# ylab, standard graphics parameter
                   main = "Msdplot", 				# title of the plot 
                   xaxt = "s", 						# should an x axis be plotted at all? (="n" if not)
                   xlabels = NULL, 					# text added as x-axis labels
                   las = 1, 						# las=1: horizontal text, las=2: vertical text (x-axis labels)
                   separate = TRUE, 				# should clusters be plotted separately
                   size = FALSE, 					# should the size of each cluster be added to the title
                   col = NULL, 						# color of the plot
                   bars = TRUE, 					# error bars
                   alpha = 50,...) 					# alpha value for color opacity
{
if (!is.matrix(x)) stop("First argument must be a matrix !")
if (is.null(at)) at=1:ncol(x)
probes = length(at)
if (is.null(xlim)) xlim=range(at)
maxp = xlim[2]
minp = xlim[1]
if (is.null(ylim)) ylim=range(x,na.rm=TRUE)
if (is.null(xlabels)) xlabels = 1:length(at)
# one cluster in one plot
if (is.null(label)){if (size){main = paste(main,"#",nrow(x))}
                    plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=main,...)
                    if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels)
                    singlemsdplot(x=x,at=at,alpha=alpha,col=col,bars=bars)}
# several clusters in one plot or several plots
if (!is.null(label)){clusternames = unique(label)
                     nrclusters = length(clusternames)
                     clustersets = split(1:nrow(x),factor(label))	
                     if (length(main)>0) main = paste(main,"\n",sep="")
                     if (separate==FALSE){if (size){main = paste(main,"#",nrow(x))}
                                          plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=main,...)
                                          if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels)}
                     if (separate==TRUE) par(mfrow=windowxy(nrclusters))
                     for (j in seq(clusternames)){if (separate==TRUE){if (length(main)==1) clustermain = paste(main,clusternames[j]," # ",length(clustersets[[j]]),sep="") else clustermain = main[j]
                                                                      plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=clustermain,...)
                                                                      if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels)}
                                                  singlemsdplot(x=x[clustersets[[j]],,drop=FALSE],at=at,alpha=alpha,col=col[j],bars=bars)
                                                 }
                    }
}

