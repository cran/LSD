demo.plotit <-
function(){
x = rlnorm(500,mean=log(16),sd=0.3)
plotsfkt = function(){linesplot(x)}
plotit(filename = "test.jpg",sw = 2,sh = 2,sres = 2,plotsfkt = plotsfkt,ww = 7,wh = 7,saveit = TRUE)
}

