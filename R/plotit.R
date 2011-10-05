

### plotting wrapper function ###


plotit = function(filename, 			# name of the plot to be saved with the format type suffix
		sw = 1, 						# scaling factor of weight
		sh = 1, 						# scaling factor of height
		sres = 1, 						# scaling factor of the resolution
		plotsfkt, 						# list of plots to be plotted
		ww = 7, 						# width of window
		wh = 7, 						# height of window
		pointsize = 12,     			# the default pointsize of plotted text, interpreted as big points (1/72 inch) for plots to be saved
		dev.pointsize = 8,				# pointsize of plotted text, interpreted as big points (1/72 inch) for display in R
		paper = "special",   			# needed only if filformat = "pdf" or "ps"
		quality = 100,     				# needed only if filformat = "jpeg"
		units = "px",        			# needed only if filformat = "jpeg", "png", "bmp" or "tiff"
		bg = "white", 					# backgroundcolor
		fileformat = "jpeg", 			# save the plot as jpeg, png, bmp, tiff, ps or pdf
		saveit = FALSE, 				# should plot be saved
		notinR = FALSE, 				# should plot be not plotted in R
		addformat = NULL 				# should plot be saved additionally in another format
)
{
	pwidth = sw*480
	pheight = sh*480
	pres = sres*72
	if (saveit){
		switch(fileformat,"jpeg" = jpeg(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, quality = quality, bg = bg,res = pres),
				"png" = png(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
				"bmp" = bmp(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
				"tiff" = tiff(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
				"ps" = postscript(file = paste(filename,".",fileformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper),
				"pdf" = pdf(file = paste(filename,".",fileformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper))
		plotsfkt()
		dev.off()
	}
	if (saveit){
		if (!is.null(addformat)){
			switch(addformat,"jpeg" = jpeg(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, quality = quality, bg = bg,res = pres),
					"png" = png(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
					"bmp" = bmp(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
					"tiff" = tiff(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
					"ps" = postscript(file = paste(filename,".",addformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper),
					"pdf" = pdf(file = paste(filename,".",addformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper))
			plotsfkt()
			dev.off()
		}
	}
	if (!notinR){
		dev.new(width = ww,height = wh,pointsize = dev.pointsize)
		plotsfkt()
	}
}


demo.plotit = function()
{
	x = rlnorm(500,meanlog=16,sdlog=0.3)
	plotsfkt = function(){linesplot(x)}
	plotit(filename = "test",sw = 2,sh = 2,sres = 2,plotsfkt = plotsfkt,ww = 7,wh = 7,saveit = TRUE)
}


#demo.plotit()



