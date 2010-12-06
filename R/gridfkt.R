gridfkt <-
function(lim, 			# limits for quadratic plots
				lty=2, 				# linetype to be used
				lwd=1,              # linewidth to be used
				addlines = TRUE,    # should fold lines be added
				folds = TRUE) 		# should labels be added to fold lines
{
	count = lim[2]-lim[1]+1
	sequ = lim[2]:lim[1]
	colorRdBu = brewer.pal(11, "RdBu")
	colfct = colorRampPalette(colorRdBu)
	colpal = colfct(count)
	abline(h=0,lty=lty,lwd=lwd)
	abline(v=0,lty=lty,lwd=lwd)
	if (addlines){for (i in 1:count){if (!(sequ[i] == 0)){abline(h=sequ[i],lty=lty,lwd=lwd,col=colpal[i])}}}
	if (folds){text(lim[1]+1,-1,"2 fold",col=colpal[which(sequ == -1)],adj=c(-.1,-.1))}
	if (folds){text(lim[1]+1,-2,"4 fold",col=colpal[which(sequ == -2)],adj=c(-.1,-.1))}
	if (folds){text(lim[1]+1,-3,"8 fold",col=colpal[which(sequ == -3)],adj=c(-.1,-.1))}
	if (addlines){for (i in 1:count){if (!(sequ[i] == 0)){abline(v=sequ[i],lty=lty,lwd=lwd,col=colpal[i])}}}
	if (folds){text(1,lim[2]-1,"2 fold",col=colpal[which(sequ == 1)],adj=c(.3,-.1))}
	if (folds){text(2,lim[2]-1,"4 fold",col=colpal[which(sequ == 2)],adj=c(.3,-.1))}
	if (folds){text(3,lim[2]-1,"8 fold",col=colpal[which(sequ == 3)],adj=c(.3,-.1))}
}

