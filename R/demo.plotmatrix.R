demo.plotmatrix <-
function()
{
show("plotmatrix")
devAskNewPage(ask = TRUE)
len = 20
x = sin(seq(0,2*pi,length=len*2))
fun = function(){n=sample(1:len,1); return(x[n:(n+len-1)])}
mat = t(replicate(7,fun(),simplify=T))
mat = mat + rnorm(length(mat))/2
plotmatrix(mat,cols=1:7,xlab="Columns of the matrix",ylab="Rows of the matrix")
}

