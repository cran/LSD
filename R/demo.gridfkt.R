demo.gridfkt <-
function()
{
	x11()
	plot(0,type="n",xlim=c(-5,5),ylim=c(-5,5),main="gridfkt",xlab="fold",ylab="fold")
	gridfkt(lim=c(-5,5))
}

