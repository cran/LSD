disco <-
function(nr = 11) 	# number of colors to be used
{
par(mfrow=c(1,1))
show("disco")
devAskNewPage(ask = TRUE)
ownpals = c("standard","heat","red","crazyred","green","crazygreen","blue","crazyblue","black","mountain","girly","jamaica","wird","wiorrd","wiocrd","wiorocgn","wicymgyl","wibugr","boxes","pies")
pals = ownpals
npal = length(ownpals)
plot(1,1,xlim=c(0,nr),ylim=c(0,npal),type="n",axes=FALSE,bty="n",xlab="",ylab="",main="own palettes",cex.main=1.5)
for (i in 1:npal){rect(xleft = 0:(nr - 1),ybottom = i - 1,xright = 1:nr,ytop = i - 0.2,col = colorpalette(pals[i],nr),border = "light grey")}
text(rep(-0.1,npal),(1:npal)-0.6,labels = pals,xpd = TRUE,adj = 1)
devAskNewPage(ask = TRUE)
brewerpals = c("ylorrd","ylorbr","ylgnbu","ylgn","reds","rdpu","purples","purd","pubugn","pubu","orrd","oranges","greys","greens","gnbu","bupu","bugn","blues","spectral","rdylgn","rdylbu","rdgy","rdbu","puor","prgn","piyg","brbg","set1","set2","set3","pastel1","pastel2","paired","dark2","accent")
pals = brewerpals
npal = length(brewerpals)
plot(1,1,xlim=c(0,nr),ylim=c(0,npal),type="n",axes=FALSE,bty="n",xlab="",ylab="",main="palettes from the RColorBrewer package",cex.main=1.5)
for (i in 1:npal){rect(xleft = 0:(nr - 1),ybottom = i - 1,xright = 1:nr,ytop = i - 0.2,col = colorpalette(pals[i],nr),border = "light grey")}
text(rep(-0.1,npal),(1:npal)-0.6,labels = pals,xpd = TRUE,adj = 1)
devAskNewPage(ask = TRUE)
rampspals = c("bl2gr","bl2gr2rd","bl2rd","bl2yl","cy2yl","gr2rd","ma2gr","matlablike","matlablike2","primarycolors","ygob")
pals = rampspals
npal = length(rampspals)
plot(1,1,xlim=c(0,nr),ylim=c(0,npal),type="n",axes=FALSE,bty="n",xlab="",ylab="",main="palettes from the colorRamps package",cex.main=1.5)
for (i in 1:npal){rect(xleft = 0:(nr - 1),ybottom = i - 1,xright = 1:nr,ytop = i - 0.2,col = colorpalette(pals[i],nr),border = "light grey")}
text(rep(-0.1,npal),(1:npal)-0.6,labels = pals,xpd = TRUE,adj = 1)
devAskNewPage(ask = TRUE)
grpals = c("standardterrain","standardtopo","standardheat","standardrainbow","standardcm")
pals = grpals
npal = length(grpals)
plot(1,1,xlim=c(0,nr),ylim=c(0,npal),type="n",axes=FALSE,bty="n",xlab="",ylab="",main="palettes from the grDevices package",cex.main=1.5)
for (i in 1:npal){rect(xleft = 0:(nr - 1),ybottom = i - 1,xright = 1:nr,ytop = i - 0.2,col = colorpalette(pals[i],nr),border = "light grey")}
text(rep(-0.1,npal),(1:npal)-0.6,labels = pals,xpd = TRUE,adj = 1)
}

