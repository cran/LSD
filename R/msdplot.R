

##############################################################################
### Msdplot: Progression plot for two-dimensional data in several clusters ###
##############################################################################


singlemsdplot = function(x, 			# data as matrix or list
                         col = NULL, 	# colors
                         alpha = 50, 	# alpha value for color opacity
                         bars = TRUE, 	# error bars
                         at = NULL) 	# at which x-positions the columns should occur
{
if (!is.matrix(x) & !is.list(x)) stop("First argument must be a matrix or a list !")
if (is.null(at)) if (is.matrix(x)){at=1:ncol(x)} else if (is.list(x)){at=1:length(x)}
probes = length(at)
if (is.null(col)){col = "darkgreen"}
if (is.matrix(x)){
	medians = apply(x,2,median,na.rm=TRUE)
	sds = apply(x,2,sd,na.rm=TRUE)
} else if (is.list(x)){
	medians = lapply(x,median,na.rm=TRUE)
	medians = sapply(medians,c)
	sds = lapply(x,sd,na.rm=TRUE)
	sds = sapply(sds,c)
}
for (i in 1:length(sds)){if (is.na(sds[i])){sds[i] = 0}}
qline = rbind(medians+sds,medians,medians-sds)
for (j in 1:2){polygon(at[c(1:probes,probes:1)],c(qline[j,],qline[j+1,probes:1]),col = convertcolor(col,alpha),lty=0)}
lines(at,qline[2,],col=col,lwd=2)
if (bars){for (j in 1:probes){lines(c(at[j],at[j]),c(medians[j]-sds[j],medians[j]+sds[j]),lwd=1,col=col)}
          for (j in 1:probes){lines(c(at[j]-0.2,at[j]+0.2),c(medians[j]+sds[j],medians[j]+sds[j]),lwd=1,col=col)}
          for (j in 1:probes){lines(c(at[j]-0.2,at[j]+0.2),c(medians[j]-sds[j],medians[j]-sds[j]),lwd=1,col=col)}}                               
}


msdplot = function(x, 								# matrix or list with numerical entries, quantiles of rows will define lines
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
if (!is.matrix(x) & !is.list(x)) stop("First argument must be a matrix or a list !")
if (is.null(at)) if (is.matrix(x)){at=1:ncol(x)} else if (is.list(x)){at=1:length(x)}
probes = length(at)
if (is.null(xlim)) xlim=range(at)
maxp = xlim[2]
minp = xlim[1]
if (is.null(ylim)) ylim=range(x,na.rm=TRUE)
if (is.null(xlabels)) xlabels = 1:length(at)
# one cluster in one plot
if (is.null(label)){if (size){main = paste(main,"#",probes)}
                    plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=main,...)
                    if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels)
                    singlemsdplot(x=x,at=at,alpha=alpha,col=col,bars=bars)}
# several clusters in one plot or several plots
if (!is.null(label)){clusternames = unique(label)
                     nrclusters = length(clusternames)
					 if (!is.matrix(x)) stop("First argument must be a matrix for multiple clusters !")
                     clustersets = split(1:nrow(x),factor(label))	
                     if (length(main)>0) main = paste(main,"\n",sep="")
                     if (separate==FALSE){if (size){main = paste(main,"#",probes)}
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


### demo.msdplot ###


demo.msdplot = function()
{
par(mfrow=c(1,1))
LSDshow("msdplot")
devAskNewPage(ask = TRUE)
at = c(2,4,8,16,32)
clus = matrix(rnorm(500,sd=0.5),ncol=5)
batch = sample(c(-8,-6,-4,-2),100,replace=TRUE)
clus = clus + cbind(0,0.25*batch,0.5*batch,0.75*batch,batch)
clus = clus - clus[,1]
clus = t(t(clus)*c(0,0.1,0.25,0.5,1))
labs = paste("cluster",kmeans(clus,4)$cluster)
msdplot(clus,label=labs,separate=FALSE,main="Alpha overlay",col=c("darkgreen","darkblue","darkred","black"),alpha=25,at=at,xlabels=at)
devAskNewPage(ask = TRUE)
msdplot(clus,label=labs,main="Clustered data",col=c("darkgreen","darkblue","darkred","black"),alpha=50,at=at,xlabels=at)
}


#demo.msdplot()



