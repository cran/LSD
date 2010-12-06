demo.heatboxplot <-
function()
{
	par(mfrow=c(1,1))
	show("heatboxplot")
	devAskNewPage(ask = TRUE)
	f = c(rnorm(200),rnorm(200)+4)
	h = rf(500,15,15)*10
	g = rnorm(300)+1
	heatboxplot(h,labels=c("unimodal"))
	devAskNewPage(ask = TRUE)
	heatboxplot(list(f=f,g=g),colpals=c("rdpu","greens"),main="rdpu and greens",labels=c("bimodal","unimodal"))
	devAskNewPage(ask = TRUE)
	heatboxplot(list(f=f,g=g),colpals=c("rdpu","greens"),main="rdpu and greens with adjust = 0.2",labels=c("bimodal","unimodal"),adjust=0.2)
	devAskNewPage(ask = TRUE)
	par(mai = c(1.02,1.02,0.82,0.42))
	heatboxplot(list(f=f,g=g),colpals=c("rdpu","greens"),horizontal=TRUE,main="rdpu and greens with horizontal = TRUE",labels=c("bimodal","unimodal"),las=2)
	devAskNewPage(ask = TRUE)
	l = list()
	for (i in 1:30){l[[i]] = rnorm(200,mean=sqrt(i^2.5),sd=1+i/2)}
	heatboxplot(l,colpal="blues",main="blues",boxonly=TRUE,adjust=0.5,quant.from=0.05,quant.to=0.95,plot.boxplot=FALSE,cexbox=1,add.quartiles=FALSE)
	legend("topleft",c("colpal = \"blues\"","boxonly = TRUE","adjust = 0.5","quant.from = 0.05","quant.to = 0.95","plot.boxplot = FALSE","cexbox = 1","add.quartiles = FALSE"))
}

