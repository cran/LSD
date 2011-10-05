

### LSDshow function ###


LSDshow = function(it){
	emptyplot()
	text(-0.7,0.15,it,pos=4,cex=pmax(1,6-log(nchar(it))))
	points(-0.7,0,pch=3,cex=8,col="red")
}


### emptyplot ###


emptyplot = function(xlim=c(-1,1),ylim=c(-1,1),...)
{
	plot(0,0,axes=FALSE,type="n",xlab="",ylab="",xlim=xlim,ylim=ylim,...)
}


### windowxy function ###


windowxy = function(windows=1) # number of windows
{
	if (is.prim(windows) & windows <= 3) return(c(1,windows))
	if (is.prim(windows) & windows > 3){a = floor(sqrt(windows))
		if (a^2==windows) return(c(a,a))
		if ( windows <= a*(a+1) ) return(c(a,a+1))
		return(c(a,a+2))
	}
	if (!is.prim(windows)){p = prime.factor(windows)
		if (length(p) == 2) return(p)
		else {while(length(p) > 2) {l = length(sort(p))
				s = sort(p)
				p = c(s[1]*s[2],s[3:length(s)])
			}
			return(sort(p))
		}
	}                                    
}


demo.windowxy = function()
{
	nr = 1:10
	mat = rbind()
	for (i in nr){mat = rbind(mat,c(i,apply(as.matrix(i),1,windowxy)))}
	colnames(mat) = c("Number of windows","1st dimension","2nd dimension")
	print(mat)
}


### isreal function ###


isreal = function(x)
{
	if (!is.numeric(x)) stop("This is not a numeric vector")
	vec = !(is.na(x) | is.nan(x) | (x==Inf) | (x==-Inf))
	return(vec)
}


### gridfkt function for quadratic plots with log2 folds ###


gridfkt = function(lim, 			# limits for quadratic plots
				lty=2, 				# linetype to be used
				lwd=1,              # linewidth to be used
				addlines = TRUE,    # should fold lines be added
				folds = TRUE) 		# should labels be added to fold lines
{
	count = lim[2]-lim[1]+1
	sequ = lim[2]:lim[1]
	colorRdBu = brewer.pal(11, "RdBu")
	colfct = colorRampPalette(colorRdBu)
	colpal = colfct(count)
	abline(h=0,lty=lty,lwd=lwd)
	abline(v=0,lty=lty,lwd=lwd)
	if (addlines){for (i in 1:count){if (!(sequ[i] == 0)){abline(h=sequ[i],lty=lty,lwd=lwd,col=colpal[i])}}}
	if (folds){text(lim[1]+1,-1,"2 fold",col=colpal[which(sequ == -1)],adj=c(-.1,-.1))}
	if (folds){text(lim[1]+1,-2,"4 fold",col=colpal[which(sequ == -2)],adj=c(-.1,-.1))}
	if (folds){text(lim[1]+1,-3,"8 fold",col=colpal[which(sequ == -3)],adj=c(-.1,-.1))}
	if (addlines){for (i in 1:count){if (!(sequ[i] == 0)){abline(v=sequ[i],lty=lty,lwd=lwd,col=colpal[i])}}}
	if (folds){text(1,lim[2]-1,"2 fold",col=colpal[which(sequ == 1)],adj=c(.3,-.1))}
	if (folds){text(2,lim[2]-1,"4 fold",col=colpal[which(sequ == 2)],adj=c(.3,-.1))}
	if (folds){text(3,lim[2]-1,"8 fold",col=colpal[which(sequ == 3)],adj=c(.3,-.1))}
}


demo.gridfkt = function()
{
	plot(0,type="n",xlim=c(-5,5),ylim=c(-5,5),main="gridfkt",xlab="fold",ylab="fold")
	gridfkt(lim=c(-5,5))
}



