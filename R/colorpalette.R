colorpalette <-
function(pal, 			# name of the palette or colorvector
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

