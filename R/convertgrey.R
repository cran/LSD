convertgrey <-
function(color, 			# vector of colors
					   alpha = NULL) 	# alpha value for color opacity
{
	colmat = col2rgb(color)
	color = apply(colmat,2,function(x){rgb(0.3*x[1]+0.59*x[2]+0.11*x[3],0.3*x[1]+0.59*x[2]+0.11*x[3],0.3*x[1]+0.59*x[2]+0.11*x[3],maxColorValue = 255)})
	if (!is.null(alpha)){color = paste(color,alpha,sep="")}
	return(color)
}

