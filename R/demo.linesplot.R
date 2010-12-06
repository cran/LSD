demo.linesplot <-
function()
{
par(mfrow=c(1,1))
show("linesplot")
devAskNewPage(ask = TRUE)
x = rlnorm(500,mean=log(16),sd=0.3)
linesplot(x,col="darkgreen",main="Linesplot of log-normal distributions")
devAskNewPage(ask = TRUE)
l = list()
for (i in 1:10){l[[i]] = rnorm(200,mean=sqrt(i^2.5),sd=1+i/2)}
linesplot(l,alpha=10,main="Linesplot of normal distributions",border="darkred",addboxes = TRUE,outline=FALSE)
}

