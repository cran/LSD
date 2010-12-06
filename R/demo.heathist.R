demo.heathist <-
function()
{
par(mfrow=c(1,1))
show("heathist")
devAskNewPage(ask = TRUE)
x = rnorm(1000,mean = sample(c(0,3),size = 1000,prob = c(.4,.6),replace = TRUE))
heathist(x,xlab="x",ylab="density",add.density=TRUE,col.rug="darkred")
devAskNewPage(ask = TRUE)
heathist(x,xlab="x",ylab="density",colpal = "matlablike")
}

