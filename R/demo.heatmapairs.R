demo.heatmapairs <-
function()
{
par(mfrow=c(1,1))
show("heatmapoints")
devAskNewPage(ask = TRUE)
points = 10^4
x <- rnorm(points/2)
x = c(x,x+2.5)
y <- x + rnorm(points,sd=0.75)
x = sign(x)*abs(x)^1.25
mat = cbind(x,y,x + rnorm(points,sd=0.5))
rownames(mat) = 1:nrow(mat)
colnames(mat) = c("x","y","z")
heatmapairs(mat,main="Heatmapairs",method="spearman",labels=c(expression(Theta),expression(Phi),expression(Omega)))
devAskNewPage(ask = TRUE)
heatmapairs(mat,main="Heatmapairs in greyscales with contour=TRUE",method="pearson",add.contour=TRUE,color.contour="red",greyscale=TRUE,labels=c(expression(Theta),expression(Phi),expression(Omega)))
}

