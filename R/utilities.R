

##########################
### plot an empty plot ###
##########################


### emptyplot ###


emptyplot = function(xlim=c(-1,1),ylim=c(-1,1),...) # just nothing
{
	plot(0,0,axes=FALSE,type="n",xlab="",ylab="",xlim=xlim,ylim=ylim,...)
}


### demo.emptyplot ###


demo.emptyplot = function()
{
	emptyplot()
}

#demo.emptyplot()


### LSDshow ###


LSDshow = function(it){
	emptyplot()
	text(-0.7,0.15,it,pos=4,cex=pmax(1,6-log(nchar(it))))
	points(-0.7,0,pch=3,cex=8,col="red")
}


###########################################
### user friendly correlation functions ###
###########################################


scor = function(x, 		# x-vector
				y,...) 	# y-vector
{
	round(cor(x,y,method="spearman",...),digits=2)
}

pcor = function(x, 		# x-vector
				y,...) 	# y-vector
{
	round(cor(x,y,method="pearson",...),digits=2)
}


###############################################################
### function to intersect two vectors in order to plot them ###
###############################################################


inter = function(x, 	# x-vector (must contain names)
				 y) 	# y-vector (must contain names)
{
	internames = intersect(names(x),names(y))
	if (length(internames) == 0) stop("VECTORS HAVE NO POINT IN COMMON !!!")
	x = x[internames]
	y = y[internames]
	if (!all(names(x) == names(y))) stop("THIS SHOULD NEVER HAPPEN !!!")
	return(list(x=x,y=y))
}


########################################################################################################################
### creates a factorization of the number of windows for plots with device partitions to be used in par(mfrow = ...) ###
########################################################################################################################


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

#demo.windowxy()


####################################################################
### computes the euclidean length of vector or the normed vector ###
####################################################################


vlength = function(x) # vector
{
	if (!is.vector(x)){print("ARGUMENT IS NOT A VECTOR !")}
	return(sqrt(sum(x^2)))
}


vnorm = function(x) # vector
{
	if (!is.vector(x)){print("ARGUMENT IS NOT A VECTOR !")}
	return(x/vlength(x))
}


############################################
### switch names and content of a vector ###
############################################


switchvector = function(vec) # vector
{
	vecnames = vec
	vec = names(vec)
	names(vec) = vecnames
	return(vec)
}


##############################
### quantile normalization ###
##############################


quantnorm = function(mat) # matrix
{
	ordmat = apply(mat,2,function(x){rank(x,ties.method = "first")})
	sortmat = apply(mat,2,sort)
	destin = apply(sortmat,1,mean)
	normmat = destin[ordmat]
	dim(normmat) = dim(mat)
	colnames(normmat) = colnames(mat)
	rownames(normmat) = rownames(mat)
	return(normmat)
}


####################################################################################################################
### rescale the columns of a matrix to standard normal distribution if they are assumed to be normal distributed ###
####################################################################################################################


rescalestandardnormal = function(mat, 				# matrix
								 protocol = TRUE) 	# should a protocol be printed
{
	if (protocol){print("means before:")
		print(apply(mat,2,mean))
		print("sds before:")
		print(apply(mat,2,sd))}
	mat = t((t(mat)-apply(mat,2,mean))/apply(mat,2,sd))
	if (protocol){print("means after:")
		print(apply(mat,2,mean))
		print("sds after:")
		print(apply(mat,2,sd))}
	return(mat)
}

rescalestdnorm = rescalestandardnormal


#############################################
### median center the columns of a matrix ###
#############################################


mediancenter = function(mat, 				# matrix
						userows = NULL, 	# the rows to be used
						usecolumns = NULL, 	# the columns to be used
						logscale = TRUE, 	# is the matrix in log-scale ?
						protocol = TRUE, 	# should a protocol be printed ?
						center = FALSE) 	# should the center be 0 (log-scale) or 1 (absolute scale)
{
	if (is.null(userows)){if (is.null(rownames(mat))) {userows = 1:nrow(mat)} else{userows = rownames(mat)}}
	if (is.null(usecolumns)){if (is.null(colnames(mat))) {usecolumns = 1:ncol(mat)} else{usecolumns = colnames(mat)}}
	if (protocol){print("medians before:")
		print(apply(mat[userows,usecolumns],2,median))}
	medians = apply(mat[userows,usecolumns],2,median)
	medianofmedians = median(medians)
	if (center){if (logscale){medianofmedians = 0} else{medianofmedians = 1}}
	if (logscale){mat = t(t(mat) - medians + medianofmedians)} else{mat = t(t(mat) / medians * medianofmedians)}
	if (protocol){print("medians after:")
		print(apply(mat[userows,usecolumns],2,median))}
	return(mat[,usecolumns])
}

medctr = mediancenter


#######################################################
### returns a boolean vector in contrast to is.real ###
#######################################################


isreal = function(x) # vector
{
	if (!is.numeric(x)) stop("This is not a numeric vector")
	vec = !(is.na(x) | is.nan(x) | (x==Inf) | (x==-Inf))
	return(vec)
}


########################################################
### gridfunction for quadratic plots with log2 folds ###
########################################################


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
	x11()
	plot(0,type="n",xlim=c(-5,5),ylim=c(-5,5),main="gridfkt",xlab="fold",ylab="fold")
	gridfkt(lim=c(-5,5))
}

#demo.gridfkt()









