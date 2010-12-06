scor <-
function(x, 		# x-vector
				y,...) 	# y-vector
{
	round(cor(x,y,method="spearman",...),digits=2)
}

