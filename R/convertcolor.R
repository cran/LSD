convertcolor <-
function(color, 			# vector of colors
						alpha = NULL) 	# alpha value for color opacity
{
	colmat = col2rgb(color)
	color = apply(colmat,2,function(x){rgb(x[1],x[2],x[3],maxColorValue = 255)})
	if (!is.null(alpha)){color = paste(color,alpha,sep="")}
	return(color)
}

