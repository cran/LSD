singleclusterplot <-
function(clusmat, 										# matrix with numerical entries, quantiles of rows will define colors
                             at = NULL, 									# at which x-positions the columns should occur 
                             fromto = c(0.05,0.95), 						# the range of quantiles that should be plotted
                             colpal = "standardheat", 						# which colorpalette should be chosen ( see disco() )
                             nrcolors = 25, 								# the number of colors to use
                             outer.col = "light grey", 						# color of the outlier lines
                             revpal = FALSE, 								# should the colorpalette be inverted
                             alpha = NULL, 									# alpha value for color opacity
							 quartiles.col = c("black","grey","grey"), 		# color of quartile lines
                             add.quartiles = TRUE) 							# should the quartile lines be plotted
{
if (!is.matrix(clusmat)) stop("First argument must be a matrix !")
if (is.null(at)) at=1:ncol(clusmat)
probes = length(at)
drawline = function(y,col="black",lwd=1,lty=1){lines(at[1:length(y)],y,type="l",col=col,lwd=lwd,lty=lty)}
if (outer.col!="none") apply(clusmat,1,drawline,col=outer.col)	
firstquart = ceiling(nrcolors/2)
nrcolpal = 2*firstquart+2
nrquants = 4*firstquart+3
if (length(colpal)==1){colpal = colorpalette(colpal,nrcolors,alpha)
                       if (revpal){colpal = rev(colpal)}
                      } else 	 colpal = colorRampPalette(colpal)(nrcolors)
colpal = colpal[c(nrcolpal:1,2:nrcolpal)]
qline = apply(clusmat,2,quantile,probs=seq(fromto[1],fromto[2],length=nrquants))
for (j in 1:(nrquants-1)){polygon(at[c(1:probes,probes:1)],c(qline[j,],qline[j+1,probes:1]),col = colpal[j],lty=0)}
if (add.quartiles){drawline(qline[2*firstquart+2,],col=quartiles.col[1],lwd=1.5) #substr(colpal[25],1,7)
		               drawline(qline[firstquart+1,],col=convertcolor(quartiles.col[2],alpha),lwd=1)
	                 drawline(qline[3*firstquart+3,],col=convertcolor(quartiles.col[3],alpha),lwd=1)}
}

