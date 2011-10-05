

####################################################
### interpolate rows of a matrix to make a movie ###
####################################################


### makemovie ###


makemovie = function(mat, 				# matrix
                     timepoints=NULL, 	# the timepoints of the rows of the matrix
                     timestep=1, 		# the timesteps between the existing timepoints
                     motionline=NULL) 	# results from timepoints and timesteps by default
{
if (!is.matrix(mat)) stop("First argument must be a matrix !")
if (sum(is.na(mat)) != 0) stop("Matrix has to be imputed beforehand !")
if (is.null(timepoints)) timepoints = 1:ncol(mat)
sorted = order(timepoints,decreasing=F)
mat = mat[,sorted]
timepoints = timepoints[sorted]
colnames(mat) = timepoints
if (is.null(motionline)){
	nrpics = ceiling(diff(range(timepoints))/timestep)+1
	motionline = seq(timepoints[1],timepoints[length(timepoints)],length=nrpics)		
}
timeline = function(x){splinefun(timepoints,x)(motionline)}
movie = t(apply(mat,1,timeline))
colnames(movie) = round(motionline,digits=2)
return(movie)
}


### demo.makemovie ###


demo.makemovie = function()
{
par(mfrow=c(1,1))
LSDshow("makemovie")
devAskNewPage(ask = TRUE)
len = 10
x = sin(seq(0,2*pi,length=len*2))
fun = function(){n=sample(1:len,1); return(x[n:(n+len-1)])}
mat = t(replicate(7,fun(),simplify=T))
mat = mat + rnorm(length(mat))/2
par(mfrow=c(1,2))
plotmatrix(mat,main="Original measurements",col=1:7,type="o",xlab="timepoints",ylab="values")
mov = makemovie(mat,timestep=0.2)
plotmatrix(mov,main="Interpolated Movie",col=1:7,type="o",xlab="timepoints",ylab="values")
}

#demo.makemovie()


