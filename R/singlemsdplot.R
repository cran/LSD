singlemsdplot <-
function(x, 			# data as matrix
                         col = NULL, 	# colors
                         alpha = 50, 	# alpha value for color opacity
                         bars = TRUE, 	# error bars
                         at = NULL) 	# at which x-positions the columns should occur
{
if (!is.matrix(x)) stop("First argument must be a matrix !")
if (is.null(at)){at=1:ncol(x)}
if (is.null(col)){col = "darkgreen"}
medians = apply(x,2,median,na.rm=TRUE)
sds = apply(x,2,sd,na.rm=TRUE)
for (i in 1:length(sds)){if (is.na(sds[i])){sds[i] = 0}}
qline = rbind(medians+sds,medians,medians-sds)
for (j in 1:2){polygon(at[c(1:ncol(x),ncol(x):1)],c(qline[j,],qline[j+1,ncol(x):1]),col = convertcolor(col,alpha),lty=0)}
lines(at,qline[2,],col=col,lwd=2)
if (bars){for (j in 1:length(at)){lines(c(at[j],at[j]),c(medians[j]-sds[j],medians[j]+sds[j]),lwd=1,col=col)}
          for (j in 1:ncol(x)){lines(c(at[j]-0.2,at[j]+0.2),c(medians[j]+sds[j],medians[j]+sds[j]),lwd=1,col=col)}
          for (j in 1:ncol(x)){lines(c(at[j]-0.2,at[j]+0.2),c(medians[j]-sds[j],medians[j]-sds[j]),lwd=1,col=col)}}                               
}

