pcor <-
function(x, 		# x-vector
				y,...) 	# y-vector
{
	round(cor(x,y,method="pearson",...),digits=2)
}

