distinctcolors <-
function(n = 20, 			# number of colors to be used
						  bw = FALSE,       # should black and white be removed
						  graphics = TRUE, 	# should x11() be called
                          maxvalue = 255) 	# maximal color value (rgb scheme)
{
lower = 1
while ((n+2) > lower^3) lower = lower + 1
valseq = seq(0,maxvalue,length.out = lower)
spl = permutations(length(valseq),3,valseq,repeats.allowed=TRUE)
if (!bw){spl = spl[-c(1,nrow(spl)),]}
spl = kmeans(spl,n)$centers
pal = apply(spl,1,function(x){rgb(x[1],x[2],x[3],maxColorValue = 255)})
if (graphics){x11()}
image(1:length(pal),1,as.matrix(1:length(pal)),col=pal,xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
return(pal)
}

