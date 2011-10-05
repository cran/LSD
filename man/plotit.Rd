\name{plotit}
\alias{plotit}
\alias{demo.plotit}
\title{Plot it}
\description{Plot plots in printable quality and all kinds of formats.}
\usage{
plotit(filename, sw = 1, sh = 1, sres = 1, plotsfkt, ww = 7, wh = 7, pointsize = 12, dev.pointsize = 8, paper = "special", quality = 100, units = "px", bg = "white", fileformat = "jpeg", saveit = FALSE, notinR = FALSE, addformat = NULL)
}
\arguments{
  \item{filename}{name of the plot to be saved with the format type suffix}
  \item{sw}{scaling factor of width}
  \item{sh}{scaling factor of height}
  \item{sres}{scaling factor of the resolution}
  \item{plotsfkt}{function of plots to be plotted}
  \item{ww}{width of window}
  \item{wh}{height of window}
  \item{pointsize}{the default pointsize of plotted text, interpreted as big points (1/72 inch) for plots to be saved}
  \item{dev.pointsize}{pointsize of plotted text, interpreted as big points (1/72 inch) for display in R}
  \item{paper}{needed only if filformat = "pdf" or "ps"}
  \item{quality}{needed only if filformat = "jpg"}
  \item{units}{needed only if filformat = "jpg", "png", "bmp" or "tiff"}
  \item{bg}{backgroundcolor}
  \item{fileformat}{save the plot as jpeg, png, bmp, tiff, ps or pdf}
  \item{saveit}{should plot be saved}
  \item{notinR}{should plot be not plotted in R}
  \item{addformat}{should plot be saved additionally in another format}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{LSD.demo.tour}}}
\examples{demo.plotit()}
\keyword{ ~kwd1 }
