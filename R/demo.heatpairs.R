demo.heatpairs <-
function()
{
par(mfrow=c(1,1))
show("heatscatterpoints")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.75)
x = sign(x)*abs(x)^1.3
mat = cbind(x,y,x + rnorm(points,sd=0.5))
colnames(mat) = c("x","y","z")
rownames(mat) = 1:nrow(mat)
heatpairs(mat,main="Heatpairs",method="spearman",labels=c(expression(Xi),expression(Lambda),expression(Delta)))
devAskNewPage(ask = TRUE)
heatpairs(mat,main="Heatpairs in greyscales with contour=TRUE",method="pearson",add.contour=TRUE,color.contour="red",greyscale=TRUE,labels=c(expression(Xi),expression(Lambda),expression(Delta)))
}

