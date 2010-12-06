\name{plotit}
\alias{plotit}
\alias{demo.plotit}
\title{Plot it}
\description{Plot plots in printable quality and all kinds of formats.}
\usage{
plotit(filename, sw = 2, sh = 2, sres = 2, plotsfkt, ww = 7, wh = 7, pointsize = 12, x11pointsize = 8, paper = "special", quality = 100, units = "px", bg = "white", cex.main = 2, cex.lab = 1.5, fileformat = "jpg", saveit = FALSE, notinR = FALSE, addformat = NULL)
}
\arguments{
  \item{filename}{\code{name of the plot to be saved with the format type suffix}}
  \item{sw}{\code{scaling factor of width}}
  \item{sh}{\code{scaling factor of height}}
  \item{sres}{\code{scaling factor of the resolution}}
  \item{plotsfkt}{\code{function of plots to be plotted}}
  \item{ww}{\code{width of window}}
  \item{wh}{\code{height of window}}
  \item{pointsize}{\code{the default pointsize of plotted text, interpreted as big points (1/72 inch) for plots to be saved}}
  \item{x11pointsize}{\code{pointsize of plotted text, interpreted as big points (1/72 inch) for display in R}}
  \item{paper}{\code{needed only if filformat = "pdf" or "ps"}}
  \item{quality}{\code{needed only if filformat = "jpg"}}
  \item{units}{\code{needed only if filformat = "jpg", "png", "bmp" or "tiff"}}
  \item{bg}{\code{backgroundcolor}}
  \item{cex.main}{\code{magnification to be used for main titles}}
  \item{cex.lab}{\code{magnification to be used for x and y labels}}
  \item{fileformat}{\code{save the plot as jpeg, png, bmp, tiff, ps or pdf}}
  \item{saveit}{\code{should plot be saved}}
  \item{notinR}{\code{should plot be not plotted in R}}
  \item{addformat}{\code{should plot be saved additionally in another format}}
}
\author{Bjoern Schwalb}
\seealso{\code{\link{LSD.demo.tour}}}
\examples{demo.plotit()}
\keyword{ ~kwd1 }
