demo.heatscatter <-
function()
{
par(mfrow=c(1,1))
show("heatscatter")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.8)
x = sign(x)*abs(x)^1.3
par(mfrow=c(2,2))
kde2dplot(x,y,main="Density Estimation (from R Graph Gallery)")
plot(x,y,pch=19,col="#00000010",main="Alpha Scatter")
heatscatter(x,y,colpal="heat")
heatscatter(x,y,colpal="heat",greyscale = TRUE,alpha=25)
devAskNewPage(ask = TRUE)
par(mfrow=c(2,2))
heatscatter(x,y,colpal="jamaica",main="jamaica",cor=FALSE)
heatscatter(x,y,colpal="bl2gr2rd",main="bl2gr2rd",cor=FALSE)
heatscatter(x,y,colpal="heat",main="greyscales with contour=TRUE",cor=FALSE,,add.contour=TRUE,color.contour="red",greyscale=TRUE)
heatscatter(x,y,colpal="spectral",main="spectral with contour=TRUE",cor=FALSE,add.contour=TRUE)
}

