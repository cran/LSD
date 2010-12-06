demo.clusterplot <-
function(){
par(mfrow=c(1,1))
show("clusterplot")
devAskNewPage(ask = TRUE)
sampls = 100
probes = 63
at = (-31:31)*14
clus = matrix(rnorm(probes*sampls,sd=1),ncol=probes)
clus = rbind( t(t(clus)+sin(1:probes/10))+1:nrow(clus)/sampls , t(t(clus)+sin(pi/2+1:probes/10))+1:nrow(clus)/sampls )
labs = paste("cluster",kmeans(clus,4)$cluster)
labsalt = paste("cluster",kmeans(clus,2)$cluster)
clusterplot(clus,main="All data",fromto=c(0,1))
devAskNewPage(ask = TRUE)
clusterplot(clus,label=labs,separate=FALSE,xaxt="n",main="Overlay",fromto=c(0.4,0.6),colpal=c("standardheat","crazyblue","crazyred","crazygreen"),outer.col="none",ylim=c(-2,3))
devAskNewPage(ask = TRUE)
clusterplot(clus,label=labsalt,separate=FALSE,xaxt="n",main="Alpha overlay",fromto=c(0.3,0.7),colpal=c("greens","purples"),outer.col="none",ylim=c(-1,2),alpha=50,revpal=TRUE)
devAskNewPage(ask = TRUE)
clusterplot(clus,label=labs,main="Clustered data",colpal=c("standardheat","crazyblue","crazyred","standardtopo"),add.quartiles=FALSE)
}

