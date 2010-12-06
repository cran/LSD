demo.heatmaplot <-
function()
{
par(mfrow=c(1,1))
show("heatmaplot")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.2)
x = sign(x)*abs(x)^1.25
par(mfrow=c(2,2))
heatmaplot(x,y,colpal="heat",main="heat",cor=FALSE)
heatmaplot(x,y,colpal="bl2gr2rd",main="bl2gr2rd",cor=FALSE)
heatmaplot(x,y,colpal="heat",main="greyscales with contour=TRUE",cor=FALSE,,add.contour=TRUE,color.contour="red",greyscale=TRUE)
heatmaplot(x,y,colpal="spectral",main="spectral with contour=TRUE",cor=FALSE,add.contour=TRUE)
}

