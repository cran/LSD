plotit <-
function(filename, 			# name of the plot to be saved with the format type suffix
                  sw = 2, 				# scaling factor of weight
                  sh = 2, 				# scaling factor of height
                  sres = 2, 			# scaling factor of the resolution
                  plotsfkt, 			# list of plots to be plotted
                  ww = 7, 				# width of window
                  wh = 7, 				# height of window
				  pointsize = 12,       # the default pointsize of plotted text, interpreted as big points (1/72 inch) for plots to be saved
				  x11pointsize = 8, 	# pointsize of plotted text, interpreted as big points (1/72 inch) for display in R
				  paper = "special",    # needed only if filformat = "pdf" or "ps"
		          quality = 100,        # needed only if filformat = "jpg"
				  units = "px",         # needed only if filformat = "jpg", "png", "bmp" or "tiff"
                  bg = "white", 		# backgroundcolor
                  cex.main = 2, 		# magnification to be used for main titles
                  cex.lab = 1.5, 		# magnification to be used for x and y labels
                  fileformat = "jpg", 	# save the plot as jpeg, png, bmp, tiff, ps or pdf
                  saveit = FALSE, 		# should plot be saved
                  notinR = FALSE, 		# should plot be not plotted in R
                  addformat = NULL 		# should plot be saved additionally in another format
                 )
{
if (substr(filename,nchar(filename)-3,nchar(filename)) == ".jpg"){filename = substr(filename,1,nchar(filename)-4)}
pwidth = sw*480
pheight = sh*480
pres = sres*72
if (saveit){switch(fileformat,"jpg" = jpeg(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, quality = quality, bg = bg,res = pres),
                              "png" = png(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
                              "bmp" = bmp(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
                              "tiff" = tiff(filename = paste(filename,".",fileformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
                              "ps" = postscript(file = paste(filename,".",fileformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper),
                              "pdf" = pdf(file = paste(filename,".",fileformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper))
            par(cex.main = cex.main)
            par(cex.lab = cex.lab)
            plotsfkt()
            dev.off()}
if (saveit){if (!is.null(addformat)){switch(addformat,"jpg" = jpeg(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, quality = quality, bg = bg,res = pres),
                                                      "png" = png(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
                                                      "bmp" = bmp(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
                                                      "tiff" = tiff(filename = paste(filename,".",addformat,sep=""), width = pwidth, height = pheight,units = units, pointsize = pointsize, bg = bg,res = pres),
                                                      "ps" = postscript(file = paste(filename,".",addformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper),
                                                      "pdf" = pdf(file = paste(filename,".",addformat,sep=""), width = ww, height = wh, pointsize = pointsize, paper = paper))
                                     par(cex.main = cex.main)
                                     par(cex.lab = cex.lab)
                                     plotsfkt()
                                     dev.off()}}
if (!notinR){x11(ww,wh,pointsize=x11pointsize)
             par(cex.main = cex.main)
             par(cex.lab = cex.lab)
             plotsfkt()}
}

