

### ellipsescatter ###


ellipsescatter = function(x,y,groups,
		colors = NULL,
		bgcol	= "darkgrey",
		scalesd = 1,
		level = 0.75,
		location = "topright",...
)
{
	if(is.null(names(groups))){names(groups) = 1:dim(summary(groups))[1]}
	grouplength = length(names(groups))
	if(is.null(colors)){colors = 1:grouplength}
	add.ellipses = function(x,y,col)
	{
		points(x,y,col=col,pch=20)
		points(ellipse(scor(x,y,use="na.or.complete"),scale=c(sd(x,na.rm=TRUE)*scalesd,sd(y,na.rm=TRUE)*scalesd),centre=c(mean(x,na.rm=TRUE),mean(y,na.rm=TRUE)),level=level),type = 'l',col=col,lwd=3)
	}
	plot(x,y,pch=20,col=bgcol,...)
	legendtext = c()
	for (k in 1:grouplength){
		add.ellipses(x[groups[[k]]],y[groups[[k]]],col=colors[k])
		legendtext = c(legendtext,paste(names(groups)[k]," (",length(x[groups[[k]]][!is.na(x[groups[[k]]])]),")",sep=""))
	}
	legend(location,legendtext,pt.bg=colors,col=rep("black",grouplength),bg="white",pch=21,cex=1)
}	


### demo.ellipsescatter ###


demo.ellipsescatter = function()
{
	par(mfrow=c(1,1))
	LSDshow("ellipsescatter")
	devAskNewPage(ask = TRUE)
	x <- rnorm(150/3)
	x = c(x,x+2,x+4)
	names(x) = 1:150
	y <- x + rnorm(150,sd=0.8)
	y[51:100] = y[51:100]*4
	x = sign(x)*abs(x)^1.3
	ellipsescatter(x,y,xlab="x vector",ylab="y vector",groups = list("Group1" = 1:50,"Group2" = 51:100,"Group3" = 101:150),colors = c("darkgreen","darkred","darkblue"),location = "topleft")
}

#demo.ellipsescatter()

