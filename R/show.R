show <-
function(it){
	emptyplot()
	text(-0.7,0.15,it,pos=4,cex=pmax(1,6-log(nchar(it))))
	points(-0.7,0,pch=3,cex=8,col="red")
}

