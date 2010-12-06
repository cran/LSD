demo.convertgrey <-
function()
{
	par(mfrow=c(1,1))
	show("convertcolor")
	devAskNewPage(ask = TRUE)
	RGB = c("red","green","blue")
	print(RGB)
	print(convertcolor(RGB))
	print(convertgrey(RGB))
	pal = list(convertgrey(RGB),convertcolor(RGB))
	plot(1,1,xlim=c(0,3),ylim=c(0,2),type="n",axes=FALSE,bty="n",xlab="",ylab="",main="greyscaled RGB")
	for (i in 1:2){rect(xleft = 0:(3 - 1),ybottom = i - 1,xright = 1:3,ytop = i - 0.2,col = pal[[i]],border = "light grey")}
	text(rep(-0.1,2),(1:2)-0.6,labels = c("greyscaled","RGB"),xpd = TRUE,adj = 1)
}

