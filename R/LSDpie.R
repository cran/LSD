

#################################################
### piecharts at arbitrary position and radii ###
#################################################


LSDpie = function(props, 				# vector giving the relations of the pie pieces (need not to be normed)
                    x = 0, 				# x-position of the piechart
                    y = 0, 				# y-position of the piechart
                    radius = 1, 		# radius of the piechart
                    cols = NULL, 		# vector of colors
                    colpal = "pies", 	# colorpalette if cols is not specified
                    edges = 100, 		# number of edges the "circle" will have
                    add = FALSE, 		# should piechart be added to an existing plot
                    xlim = NULL, 		# xlimits, standard graphics parameter
                    ylim = NULL, 		# ylimits, standard graphics parameter
                    main = "Piecharts", # title of the plot
                    alpha = NULL,...) 	# alpha value for color opacity
{
xx = sin(-(0:edges)/edges*2*pi)*radius
yy = cos(-(0:edges)/edges*2*pi)*radius
n = length(props)
if (is.null(cols)) cols = colorpalette(colpal,n,alpha)	
phi = cumsum(props)/sum(props)*edges
phi = c(0,round(phi))+1
if (!add){emptyplot(xlim=xlim,ylim=ylim,main=main,...)}	
for (j in 1:n){xvec = c(x,x+xx[phi[j]:phi[j+1]])
		           yvec = c(y,y+yy[phi[j]:phi[j+1]])
               polygon(xvec,yvec,col=cols[j],border=NA)
              }
}


demo.LSDpie = function()
{
par(mfrow=c(1,1))
LSDshow("piechart")
devAskNewPage(ask = TRUE)
probs = c(1,2,3,4,5,6)
LSDpie(probs,xlim=c(-5,5),ylim=c(-5,5),colpal="heat")
probs = c(3,4,1,2,7,9)
LSDpie(probs,add=TRUE,radius=1.5,colpal="girly",x=3,y=3)
probs = c(3,4,1,2,7,9,13,4,5)
LSDpie(probs,add=TRUE,radius=2,colpal="mountain",x=-3,y=-2)
probs = c(3,6,2,1,5,6,20,8,7,5,4,3,3)
LSDpie(probs,add=TRUE,radius=1.7,colpal="standard",x=-3.5,y=3.5)
probs = c(2,4,7,9,20,30,4,7,6,9,23)
LSDpie(probs,add=TRUE,radius=2,colpal="primarycolors",x=3.25,y=-3.25)
}


#demo.LSDpie()



