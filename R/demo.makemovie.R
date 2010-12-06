demo.makemovie <-
function()
{
par(mfrow=c(1,1))
show("makemovie")
devAskNewPage(ask = TRUE)
len = 10
x = sin(seq(0,2*pi,length=len*2))
fun = function(){n=sample(1:len,1); return(x[n:(n+len-1)])}
mat = t(replicate(7,fun(),simplify=T))
mat = mat + rnorm(length(mat))/2
par(mfrow=c(1,2))
plotmatrix(mat,main="Original measurements",col=1:7,type="o",xlab="timepoints",ylab="values")
mov = makemovie(mat,timestep=0.2)
plotmatrix(mov,main="Interpolated Movie",col=1:7,type="o",xlab="timepoints",ylab="values")
}

