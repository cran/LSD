demo.comparisonplot <-
function()
{
par(mfrow=c(1,1))
show("comparisonplot")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+4)
y <- x + rnorm(points,sd=0.8)
x = sign(x)*abs(x)^1.3
cplot(x,y,xlab="x vector",ylab="y vector",histbreaks=30,pch=20)
devAskNewPage(ask = TRUE)
cplot(x,y,xlab="x vector",ylab="y vector",histbreaks=30,colpal="black",pimp=TRUE,add.density = TRUE,col.density = "darkred")
devAskNewPage(ask = TRUE)
cplot(x,y,xlab="x vector",ylab="y vector",histbreaks=30,colpal="matlablike",pimp=TRUE,adjust=0.5)
}

