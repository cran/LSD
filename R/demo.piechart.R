demo.piechart <-
function()
{
par(mfrow=c(1,1))
show("piechart")
devAskNewPage(ask = TRUE)
probs = c(1,2,3,4,5,6)
piechart(probs,xlim=c(-5,5),ylim=c(-5,5),colpal="heat")
probs = c(3,4,1,2,7,9)
piechart(probs,add=TRUE,radius=1.5,colpal="girly",x=3,y=3)
probs = c(3,4,1,2,7,9,13,4,5)
piechart(probs,add=TRUE,radius=2,colpal="mountain",x=-3,y=-2)
probs = c(3,6,2,1,5,6,20,8,7,5,4,3,3)
piechart(probs,add=TRUE,radius=1.7,colpal="standard",x=-3.5,y=3.5)
probs = c(2,4,7,9,20,30,4,7,6,9,23)
piechart(probs,add=TRUE,radius=2,colpal="primarycolors",x=3.25,y=-3.25)
}

