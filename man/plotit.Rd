% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LSD.plotit.r
\name{plotit}
\alias{plotit}
\alias{LSD.plotit}
\title{Plotting wrapper function to plot plots in printable quality and all kinds of formats}
\usage{
plotit(
  filename,
  sw = 1,
  sh = 1,
  sres = 1,
  plotsfkt,
  ww = 7,
  wh = 7,
  pointsize = 12,
  dev.pointsize = 8,
  paper = "special",
  quality = 100,
  units = "px",
  bg = "white",
  fileformat = "jpeg",
  saveit = FALSE,
  notinR = FALSE,
  addformat = NULL
)
}
\arguments{
\item{filename}{name of the plot to be saved with the format type suffix.}

\item{sw}{scaling factor of weight.}

\item{sh}{scaling factor of height.}

\item{sres}{scaling factor of the resolution.}

\item{plotsfkt}{list of plots to be plotted.}

\item{ww}{width of window.}

\item{wh}{height of window.}

\item{pointsize}{the default pointsize of plotted text, interpreted as big points (1/72 inch) for plots to be saved.}

\item{dev.pointsize}{pointsize of plotted text, interpreted as big points (1/72 inch) for display in R.}

\item{paper}{needed only if filformat = "pdf" or "ps".}

\item{quality}{needed only if filformat = "jpeg".}

\item{units}{needed only if filformat = "jpeg", "png", "bmp" or "tiff".}

\item{bg}{backgroundcolor.}

\item{fileformat}{save the plot as "pdf", "ps", "jpeg", "png", "bmp" or "tiff".}

\item{saveit}{should plot be saved.}

\item{notinR}{should plot be not plotted in R.}

\item{addformat}{should plot be saved additionally in another format ("pdf", "ps", "jpeg", "png", "bmp" or "tiff").}
}
\description{
Plotting wrapper function to save plots in R as "pdf", "ps", "jpeg", "png", "bmp" or "tiff".
}
\examples{
data(homer)

plotsfkt = function(){
		colpal = c("white","black","yellow","wheat3")
		align(homer,colpal = colpal,main = "D'OH!",asp = 1,axes = FALSE)
}

# choose 'saveit = TRUE' for saving the plot in the specified fileformat #

# choose 'notinR = TRUE' to suppress plotting in R #

plotit("homer",sw = 2,sh = 2,sres = 2,plotsfkt,saveit = FALSE,notinR = FALSE,fileformat = "jpeg")

plotit("homer",sw = 2,sh = 2,sres = 2,plotsfkt,saveit = FALSE,notinR = FALSE,fileformat = "png")

plotit("homer",sw = 2,sh = 2,sres = 2,plotsfkt,saveit = FALSE,notinR = FALSE,fileformat = "bmp")

plotit("homer",sw = 2,sh = 2,sres = 2,plotsfkt,saveit = FALSE,notinR = FALSE,fileformat = "tiff")

plotit("homer",sw = 2,sh = 2,sres = 2,plotsfkt,saveit = FALSE,notinR = FALSE,fileformat = "ps")

plotit("homer",sw = 2,sh = 2,sres = 2,plotsfkt,saveit = FALSE,notinR = FALSE,fileformat = "pdf")
}
\seealso{
\code{\link{demotour}}, \code{\link{disco}}, \code{\link{colorpalette}}
}
\author{
Bjoern Schwalb
}
\keyword{bmp}
\keyword{jpeg}
\keyword{pdf}
\keyword{plot}
\keyword{png}
\keyword{ps}
\keyword{tiff}
