

############################################################################################################
### provides color palettes of several R packages (grDevices, RColorBrewer, colorRamps and own palettes) ###
############################################################################################################


### colorpalette ###


colorpalette = function(pal, 			# name of the palette or colorvector
                        nrcol = NULL, 	# number of colors to be used
                        alpha = NULL) 	# alpha value for color opacity
{
if (length(pal) > 1){palette = pal}
else{palette = switch(pal,			
					 ### own palettes 
                     standard = c("white","light yellow","yellow","gold","brown"),
                     heat = c("grey","dark blue","red","orange","gold"),
                     red = c("#E0C6C6","red"),
                     crazyred =  c( "#940000","#A50000","#FF5C5C","#FFB9B9"),
                     green = c("#C6E0C6","green"),
                     crazygreen = c("dark green","#009700","green","#C0F5D0"),
                     blue = c("#C6C6E0","blue"),
                     crazyblue = c("dark blue","blue","#7390EE","light blue"),
                     black = c("#E5E5E5","black"),
                     mountain = c("light green","dark green","black","dark grey","#F0F0F0"),
                     girly = c("violet","violetred","violetred1","purple","purple3"),
                     jamaica = c("red","yellow","green"),          
                     wird = c("white","darkred"),
                     wiorrd = c("white","darkorange","darkred"),
                     wiocrd = c("white","darkorchid4","darkred"),
                     wiorocgn = c("white","orange","darkorange","darkorchid1","darkorchid4","darkorchid1","green","darkgreen","darkgreen"),
                     wicymgyl = c("white","cyan","darkcyan","magenta","darkmagenta","magenta","yellow","yellow2","lawngreen"),
                     wibugr = c("white","skyblue1","skyblue4","slateblue1","slateblue4","slateblue1","slategray1","slategray2","slategray4"),
                     boxes = c("darkgreen","cornflowerblue","yellow","darkred","darkorchid"),
                     pies = c("red","yellow","green","purple","blue"),
					 colorblind = c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7"),
                     ### palettes from the RColorBrewer package
                     ylorrd = brewer.pal(9,"YlOrRd"),
                     ylorbr = brewer.pal(9,"YlOrBr"),
                     ylgnbu = brewer.pal(9,"YlGnBu"),
                     ylgn = brewer.pal(9,"YlGn"),
                     reds = brewer.pal(9,"Reds"),
                     rdpu = brewer.pal(9,"RdPu"),
                     purples = brewer.pal(9,"Purples"),
                     purd = brewer.pal(9,"PuRd"),
                     pubugn = brewer.pal(9,"PuBuGn"),
                     pubu = brewer.pal(9,"PuBu"),
                     orrd = brewer.pal(9,"OrRd"),
                     oranges = brewer.pal(9,"Oranges"),
                     greys = brewer.pal(9,"Greys"),
                     greens = brewer.pal(9,"Greens"),
                     gnbu = brewer.pal(9,"GnBu"),
                     bupu = brewer.pal(9,"BuPu"),
                     bugn = brewer.pal(9,"BuGn"),
                     blues = brewer.pal(9,"Blues"),
                     spectral = brewer.pal(11,"Spectral"),
                     rdylgn = brewer.pal(11,"RdYlGn"),
                     rdylbu = brewer.pal(11,"RdYlBu"),
                     rdgy = brewer.pal(11,"RdGy"),
                     rdbu = brewer.pal(11,"RdBu"),
                     puor = brewer.pal(11,"PuOr"),
                     prgn = brewer.pal(11,"PRGn"),
                     piyg = brewer.pal(11,"PiYG"),
                     brbg = brewer.pal(11,"BrBG"),
                     set1 = brewer.pal(9,"Set1"),
                     set2 = brewer.pal(8,"Set2"),
                     set3 = brewer.pal(12,"Set3"),
                     pastel1 = brewer.pal(9,"Pastel1"),
                     pastel2 = brewer.pal(8,"Pastel2"),
                     paired = brewer.pal(12,"Paired"),
                     dark2 = brewer.pal(8,"Dark2"),
                     accent = brewer.pal(8,"Accent"),
                     ### palettes from the grDevices package
                     standardterrain = terrain.colors(9),
                     standardtopo = topo.colors(9),
                     standardheat = heat.colors(9),
                     standardrainbow = rainbow(9,start=0.7,end=0.1),
                     standardcm = cm.colors(9),
                     ### palettes from the colorRamps package
                     bl2gr = blue2green(11),
                     bl2gr2rd = blue2green2red(11),
                     bl2rd = blue2red(11),
                     bl2yl = blue2yellow(11),
                     cy2yl = cyan2yellow(11),
                     gr2rd = green2red(11),
                     ma2gr = magenta2green(11),
                     matlablike = matlab.like(11),
                     matlablike2 = matlab.like2(11),
                     primarycolors = primary.colors(11),
                     ygob = ygobb(11),
                     pal 
                     )}
if (is.null(palette)){stop(paste(pal,"is not a valid palette name for colorpalette"))}
if (!is.null(nrcol)){colfct=colorRampPalette(palette)
                     palette = colfct(nrcol)}
if (!is.null(alpha)){palette = convertcolor(palette,alpha)}
return(palette)
}


### display.colorpalette ###


display.colorpalette = function(name, 			# name of the colorpalette
                                n = NULL, 		# number of colors to be used
                                alpha = NULL) 	# alpha value for color opacity
{
if (is.null(n)){n = length(colorpalette(name,n,alpha))}
if (length(name) > 1){main = paste("From",name[1],"to",name[length(name)])} else {main = paste(name)}
image(1:n,1,as.matrix(1:n),col=colorpalette(name,n,alpha),xlab="",ylab="",xaxt="n",yaxt="n",bty="n",main=main)
}

demo.display.colorpalette = function(){
	par(mfrow=c(1,1))
	LSDshow("display.colorpalette")
	devAskNewPage(ask = TRUE)
	display.colorpalette("rdbu",20)
}

#demo.display.colorpalette()


### display.all.colorpalette ###


display.all.colorpalette = function(nr = 11) 	# number of colors to be used
{
par(mfrow=c(1,1))
LSDshow("disco")
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

disco = display.all.colorpalette

#display.all.colorpalette()


### distinctcolors ###


distinctcolors = function(n = 20, 			# number of colors to be used
						  bw = FALSE,       # should black and white be removed
						  show = FALSE, 	# should colors be shown
                          maxvalue = 255) 	# maximal color value (rgb scheme)
{
lower = 1
while ((n+2) > lower^3) lower = lower + 1
valseq = seq(0,maxvalue,length.out = lower)
spl = permutations(length(valseq),3,valseq,repeats.allowed=TRUE)
if (!bw){spl = spl[-c(1,nrow(spl)),]}
spl = kmeans(spl,n)$centers
pal = apply(spl,1,function(x){rgb(x[1],x[2],x[3],maxColorValue = 255)})
if (show){
	image(1:length(pal),1,as.matrix(1:length(pal)),col=pal,xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
}
return(pal)
}


demo.distinctcolors = function()
{
	par(mfrow=c(1,1))
	LSDshow("distinctcolors")
	devAskNewPage(ask = TRUE)
	distinctcolors(show = TRUE)
}

#demo.distinctcolors()


### convert R colors to hexadecimal representation ###


convertcolor = function(color, 			# vector of colors
						alpha = NULL) 	# alpha value for color opacity
{
	colmat = col2rgb(color)
	color = apply(colmat,2,function(x){rgb(x[1],x[2],x[3],maxColorValue = 255)})
	if (!is.null(alpha)){color = paste(color,alpha,sep="")}
	return(color)
}


### convert colorpalette to grayscales ###


convertgrey = function(color, 			# vector of colors
					   alpha = NULL) 	# alpha value for color opacity
{
	colmat = col2rgb(color)
	color = apply(colmat,2,function(x){rgb(0.3*x[1]+0.59*x[2]+0.11*x[3],0.3*x[1]+0.59*x[2]+0.11*x[3],0.3*x[1]+0.59*x[2]+0.11*x[3],maxColorValue = 255)})
	if (!is.null(alpha)){color = paste(color,alpha,sep="")}
	return(color)
}


demo.convertcolor = function()
{
	par(mfrow=c(1,1))
	LSDshow("convertcolor")
	devAskNewPage(ask = TRUE)
	RGB = c("red","green","blue")
	print(RGB)
	print(convertcolor(RGB))
	print(convertgrey(RGB))
	pal = list(convertgrey(RGB),convertcolor(RGB))
	plot(1,1,xlim=c(0,3),ylim=c(0,2),type="n",axes=FALSE,bty="n",xlab="",ylab="",main="greyscaled RGB")
	for (i in 1:2){rect(xleft = 0:(3 - 1),ybottom = i - 1,xright = 1:3,ytop = i - 0.2,col = pal[[i]],border = "light grey")}
	text(rep(-0.1,2),(1:2)-0.6,labels = c("greyscaled","RGB"),xpd = TRUE,adj = 1)
}

demo.convertgrey = demo.convertcolor

#demo.convertcolor()


