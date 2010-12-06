demo.windowxy <-
function()
{
	nr = 1:10
	mat = rbind()
	for (i in nr){mat = rbind(mat,c(i,apply(as.matrix(i),1,windowxy)))}
	colnames(mat) = c("Number of windows","1st dimension","2nd dimension")
	print(mat)
}

