demo.msdplot <-
function()
{
par(mfrow=c(1,1))
show("msdplot")
devAskNewPage(ask = TRUE)
at = c(2,4,8,16,32)
clus = matrix(rnorm(500,sd=0.5),ncol=5)
batch = sample(c(-8,-6,-4,-2),100,replace=TRUE)
clus = clus + cbind(0,0.25*batch,0.5*batch,0.75*batch,batch)
clus = clus - clus[,1]
clus = t(t(clus)*c(0,0.1,0.25,0.5,1))
labs = paste("cluster",kmeans(clus,4)$cluster)
msdplot(clus,label=labs,separate=FALSE,main="Alpha overlay",col=c("darkgreen","darkblue","darkred","black"),alpha=25,at=at,xlabels=at)
devAskNewPage(ask = TRUE)
msdplot(clus,label=labs,main="Clustered data",col=c("darkgreen","darkblue","darkred","black"),alpha=50,at=at,xlabels=at)
}

