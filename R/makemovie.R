makemovie <-
function(mat, 				# matrix
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

