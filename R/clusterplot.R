

###############################################################################################################
### Clusterplot: Progression plot for two-dimensional data in several clusters, briefly a boxplot over time ###
###############################################################################################################


### singleclusterplot ###


singleclusterplot = function(clusmat, 				# matrix or list with numerical entries, quantiles of rows will define colors
		at = NULL, 									# at which x-positions the columns should occur 
		fromto = c(0.05,0.95), 						# the range of quantiles that should be plotted
		colpal = "standardheat", 					# which colorpalette should be chosen ( see disco() )
		nrcolors = 25, 								# the number of colors to use
		outer.col = "light grey", 					# color of the outlier lines
		revpal = FALSE, 							# should the colorpalette be inverted
		alpha = NULL, 								# alpha value for color opacity
		quartiles.col = c("black","grey","grey"), 	# color of quartile lines
		add.quartiles = TRUE, 						# should the quartile lines be plotted
		smooth = FALSE,								# should quantiles be smoothed
		df = 3)										# degrees of freedom
{
if (!is.matrix(clusmat) & !is.list(clusmat)) stop("First argument must be a matrix or a list !")
if (is.null(at)) if (is.matrix(clusmat)){at=1:ncol(clusmat)} else if (is.list(clusmat)){at=1:length(clusmat)}
probes = length(at)
drawline = function(y,col="black",lwd=1,lty=1){lines(at[1:length(y)],y,type="l",col=col,lwd=lwd,lty=lty)}
if (is.matrix(clusmat)){if (outer.col!="none") apply(clusmat,1,drawline,col=outer.col)}
firstquart = ceiling(nrcolors/2)
nrcolpal = 2*firstquart+2
nrquants = 4*firstquart+3
if (length(colpal)==1){colpal = colorpalette(colpal,nrcolors,alpha)
                       if (revpal){colpal = rev(colpal)}
                      } else 	 colpal = colorRampPalette(colpal)(nrcolors)
colpal = colpal[c(nrcolpal:1,2:nrcolpal)]
if (is.matrix(clusmat)){
	qline = apply(clusmat,2,quantile,probs=seq(fromto[1],fromto[2],length=nrquants),na.rm=TRUE)
} else if (is.list(clusmat)){
	qline = lapply(clusmat,quantile,probs=seq(fromto[1],fromto[2],length=nrquants),na.rm=TRUE)
	qline = sapply(qline,c)
}
if (smooth){
	for (i in 1:nrow(qline)){
		spl = smooth.spline(1:ncol(qline),qline[i,],df=df)
		qline[i,] = spl$y
	}
}
for (j in 1:(nrquants-1)){polygon(at[c(1:probes,probes:1)],c(qline[j,],qline[j+1,probes:1]),col = colpal[j],lty=0)}
if (add.quartiles){drawline(qline[2*firstquart+2,],col=quartiles.col[1],lwd=2)
		               drawline(qline[firstquart+1,],col=convertcolor(quartiles.col[2],alpha),lwd=2)
	                 drawline(qline[3*firstquart+3,],col=convertcolor(quartiles.col[3],alpha),lwd=2)}
}


### clusterplot ###


clusterplot = function(clusmat, 					# matrix or list with numerical entries, quantiles of rows will define colors
		label = NULL, 								# if multiple clusters should be plotted: vector with labels, which assigns rows to clusters
		at = NULL, 									# at which x-positions the columns should occur
		main = "Clusterplot", 						# the title(s) of the plot(s)
		xlim = NULL, 								# xlimits, standard graphics parameter
		ylim = NULL, 								# ylimits, standard graphics parameter
		xlab = "Columns",							# x-axis legend, standard graphics parameter
		ylab = "Rows",								# y-axis legend, standard graphics parameter 
		xaxt = "s", 								# should an x axis be plotted at all? (="n" if not)
		xlabels = NULL, 							# text added as x-axis labels
		las = 1, 									# las=1: horizontal text, las=2: vertical text (x-axis labels)
		fromto = c(0.05,0.95), 						# the range of quantiles that should be plotted
		colpal = "standardheat",	 				# colorpalettes assigned to sorted labels ( see disco() )
		nrcolors = 25, 								# the number of colors to use
		outer.col = "light grey", 					# color of the outlier lines
		quartiles.col = c("black","grey","grey"), 	# color of quartile lines
		add.quartiles = TRUE, 						# should the quartile lines be plotted
		separate = TRUE, 							# should clusters be plotted in subsequent figures
		revpal = FALSE, 							# should the colorpalette be inverted
		size = TRUE, 								# should the size of each cluster be added to the title
		alpha = NULL, 								# alpha value for color opacity
		smooth = FALSE,								# should quantiles be smoothed
		df = 3,...)									# degrees of freedom
{
if (!is.matrix(clusmat) & !is.list(clusmat)) stop("First argument must be a matrix or a list !")
if (is.matrix(clusmat)){
	if (sum(is.na(clusmat)) > 0) print("Be careful: Your data contains NAs !")
} else if (is.list(clusmat)){
	if (sum(is.na(unlist(clusmat))) > 0) print("Be careful: Your data contains NAs !")
}
if (is.null(at)) if (is.matrix(clusmat)){at=1:ncol(clusmat)} else if (is.list(clusmat)){at=1:length(clusmat)}
probes = length(at)
if (is.null(xlim)) xlim=c(min(at),max(at))
maxp = xlim[2]
minp = xlim[1]
if (is.null(ylim)) if (is.matrix(clusmat)){ylim=c(min(clusmat,na.rm=TRUE),max(clusmat,na.rm=TRUE))} else if (is.list(clusmat)){ylim=c(min(unlist(clusmat),na.rm=TRUE),max(unlist(clusmat),na.rm=TRUE))}
if (is.null(xlabels)) xlabels = 1:length(at)
# one cluster in one plot
if (is.null(label)){if (size){main = paste(main,"#",nrow(clusmat))}
                    plot(xlim,ylim,type="n",xaxt="n",xlab=xlab,ylab=ylab,main=main,...)
                    if (xaxt!="n") axis(side=1,las=las,at=at,labels=xlabels,...)
                    singleclusterplot(clusmat=clusmat,at=at,fromto=fromto,colpal=colpal,nrcolors=nrcolors,outer.col=outer.col,add.quartiles=add.quartiles,quartiles.col=quartiles.col,revpal=revpal,alpha=alpha,smooth=smooth,df=df)}
# several clusters in one plot or several plots
if (!is.null(label)){clusternames = sort(unique(label))
                     nrclusters = length(clusternames)
					 if (!is.matrix(clusmat)) stop("First argument must be a matrix for multiple clusters !")
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
                                                  singleclusterplot(clusmat=clusmat[clustersets[[j]],,drop=FALSE],at=at,fromto=fromto,colpal=colpal[[j]],nrcolors=nrcolors,outer.col=outer.col,add.quartiles=add.quartiles,quartiles.col=quartiles.col,revpal=revpal,alpha=alpha,smooth=smooth,df=df)
                                                 }
                    }
}


### demo.clusterplot ###


demo.clusterplot = function(){
par(mfrow=c(1,1))
LSDshow("clusterplot")
devAskNewPage(ask = TRUE)
sampls = 100
probes = 63
at = (-31:31)*14
clus = matrix(rnorm(probes*sampls,sd=1),ncol=probes)
clus = rbind( t(t(clus)+sin(1:probes/10))+1:nrow(clus)/sampls , t(t(clus)+sin(pi/2+1:probes/10))+1:nrow(clus)/sampls )
labs = paste("cluster",kmeans(clus,4)$cluster)
labsalt = paste("cluster",kmeans(clus,2)$cluster)
clusterplot(clus,main="All data",fromto=c(0,1))
devAskNewPage(ask = TRUE)
clusterplot(clus,label=labs,separate=FALSE,xaxt="n",main="Overlay",fromto=c(0.4,0.6),colpal=c("standardheat","crazyblue","crazyred","crazygreen"),outer.col="none",ylim=c(-2,3))
devAskNewPage(ask = TRUE)
clusterplot(clus,label=labsalt,separate=FALSE,xaxt="n",main="Alpha overlay",fromto=c(0.3,0.7),colpal=c("greens","purples"),outer.col="none",ylim=c(-1,2),alpha=50,revpal=TRUE)
devAskNewPage(ask = TRUE)
clusterplot(clus,label=labs,main="Clustered data",colpal=c("standardheat","crazyblue","crazyred","standardtopo"),add.quartiles=FALSE)
}


#demo.clusterplot()



