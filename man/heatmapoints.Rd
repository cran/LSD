\name{heatmapoints}
\alias{heatmapoints}
\alias{demo.heatmapoints}
\title{Heat MA points}
\description{A colored MA plot based on a 2d density computed by kde2d to be added to an existing plot.}
\usage{
heatmapoints(x, y, pch = 19, cexplot = 0.5, ncol = 30, grid = 100, colpal = "heat", xlim = NULL, ylim = NULL, alpha = NULL, add.contour = FALSE, nlevels = 10, color.contour = "black", add.line = TRUE, color.line = "darkgrey", lwd = 2, greyscale = FALSE, ...)
}
\arguments{
  \item{x}{\code{x-vector}}
  \item{y}{\code{y-vector}}
  \item{pch}{\code{plotting character}}
  \item{cexplot}{\code{cex of the points}}
  \item{ncol}{\code{the number of colors to use}}
  \item{grid}{\code{size of the grid ( see kde2d )}}
  \item{colpal}{\code{which colorpalette should be chosen ( see disco() )}}
  \item{xlim}{\code{xlimits, standard graphics parameter}}
  \item{ylim}{\code{ylimits, standard graphics parameter}}
  \item{alpha}{\code{alpha value for color opacity}}
  \item{add.contour}{\code{should the contour lines be added}}
  \item{nlevels}{\code{number of levels of the contour lines}}
  \item{color.contour}{\code{color of the contour lines}}
  \item{add.line}{\code{add horizontal line to the plot}}
  \item{color.line}{\code{color of the horizontal line}}
  \item{lwd}{\code{lwd, standard graphics parameter}}
  \item{greyscale}{\code{should the colorpalette be in greyscales}}
  \item{\dots}{\code{additional parameters to be passed to points and plot}}
}
\author{Bjoern Schwalb, Achim Tresch}
\seealso{\code{\link{heatmapairs}},\code{\link{heatmaplot}},\code{\link{LSD.demo.tour}}}
\examples{demo.heatmapoints()}
\keyword{aplot}
