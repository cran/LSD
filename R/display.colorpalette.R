display.colorpalette <-
function(name, 			# name of the colorpalette
                                n = NULL, 		# number of colors to be used
                                alpha = NULL) 	# alpha value for color opacity
{
if (is.null(n)){n = length(colorpalette(name,n,alpha))}
if (length(name) > 1){main = paste("From",name[1],"to",name[length(name)])} else {main = paste(name)}
image(1:n,1,as.matrix(1:n),col=colorpalette(name,n,alpha),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",main=main)
}

